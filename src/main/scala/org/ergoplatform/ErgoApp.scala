package org.ergoplatform

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler}
import akka.stream.ActorMaterializer
import org.ergoplatform.http._
import org.ergoplatform.http.api._
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local._
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ModeFeature}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Args, ErgoSettings, NetworkType}
import scorex.core.api.http._
import scorex.core.app.{Application, ScorexContext}
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.network.{NetworkControllerRef, PeerFeature, UPnP, UPnPGateway}
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.io.Source
import scala.util.{Failure, Success}

import kamon.Kamon

class ErgoApp(args: Args) extends ScorexLogging {

  private var ergoSettings: ErgoSettings = ErgoSettings.read(args)

  implicit private def settings: ScorexSettings = ergoSettings.scorexSettings

  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler

  implicit def rejectionHandler: RejectionHandler = ApiRejectionHandler.rejectionHandler

  implicit private val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit private val executionContext: ExecutionContext = actorSystem.dispatcher

  ergoSettings = ergoSettings.bootstrapSettingsOpt match {
    case Some(bs) if isEmptyState =>
      log.info("Entering coordinated network bootstrap procedure ..")
      val (npmProof, genesisDigest) =
        new BootstrapController(bs).waitForBootSettings()
      log.info("Boot settings received. Starting the node ..")
      ergoSettings.copy(
        chainSettings = ergoSettings.chainSettings.copy(
          noPremineProof = npmProof,
          genesisStateDigestHex = genesisDigest
        )
      )
    case _ =>
      ergoSettings
  }

  private val features: Seq[PeerFeature] = Seq(ModeFeature(ergoSettings.nodeSettings))

  private val timeProvider = new NetworkTimeProvider(settings.ntp)

  private val upnpGateway: Option[UPnPGateway] =
    if (settings.network.upnpEnabled) UPnP.getValidGateway(settings.network) else None
  upnpGateway.foreach(_.addPort(settings.network.bindAddress.getPort))

  //an address to send to peers
  private val externalSocketAddress: Option[InetSocketAddress] =
    settings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, settings.network.bindAddress.getPort))
    }

  private val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)
    val featureSerializers: PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap
    Seq(
      GetPeersSpec,
      new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects),
      invSpec,
      requestModifierSpec,
      modifiersSpec
    )
  }

  private val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)

  private val scorexContext = ScorexContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  private val peerManagerRef = PeerManagerRef("peerManager", settings, scorexContext)

  private val networkControllerRef: ActorRef = NetworkControllerRef(
    "networkController", settings.network, peerManagerRef, scorexContext)

  private val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, "nodeViewHolder")

  private val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef, "readerHolder")

  // Create an instance of ErgoMiner actor if "mining = true" in config
  private val minerRefOpt: Option[ActorRef] =
    if (ergoSettings.nodeSettings.mining) {
      Some(ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider, name = "miner"))
    } else {
      None
    }

  private val statsCollectorRef: ActorRef =
    ErgoStatsCollectorRef(readersHolderRef, networkControllerRef, ergoSettings, timeProvider, "statsCollector")

  private val nodeViewSynchronizer: ActorRef =
    ErgoNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, ErgoSyncInfoMessageSpec,
      settings.network, timeProvider, "nodeViewSynchronizer")

  private val apiRoutes: Seq[ApiRoute] = Seq(
    EmissionApiRoute(ergoSettings),
    ErgoUtilsApiRoute(ergoSettings),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi),
    InfoApiRoute(statsCollectorRef, settings.restApi, timeProvider),
    BlocksApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi),
    WalletApiRoute(readersHolderRef, nodeViewHolderRef, ergoSettings),
    UtxoApiRoute(readersHolderRef, settings.restApi),
    ScriptApiRoute(readersHolderRef, ergoSettings)
  ) ++ minerRefOpt.map(minerRef => MiningApiRoute(minerRef, ergoSettings)).toSeq

  private val swaggerRoute = SwaggerRoute(settings.restApi, swaggerConfig)
  private val panelRoute = NodePanelRoute()

  private val httpService = ErgoHttpService(apiRoutes, swaggerRoute, panelRoute)

  // Run mining immediately, i.e. without syncing if mining = true and offlineGeneration = true
  // Useful for local blockchains (devnet)
  if (ergoSettings.nodeSettings.mining && ergoSettings.nodeSettings.offlineGeneration) {
    require(minerRefOpt.isDefined, "Miner does not exist but mining = true in config")
    minerRefOpt.get ! StartMining
  }

  private val actorsToStop: Seq[ActorRef] = Seq(
    peerManagerRef,
    networkControllerRef,
    readersHolderRef,
    nodeViewSynchronizer,
    statsCollectorRef,
    nodeViewHolderRef
  ) ++ minerRefOpt.toSeq

  sys.addShutdownHook(ErgoApp.shutdown(actorSystem, actorsToStop))

  if (!ergoSettings.nodeSettings.stateType.requireProofs) {
    MempoolAuditorRef(nodeViewHolderRef, networkControllerRef, ergoSettings)
  }

  private def swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  private def run(): Unit = {
    require(settings.network.agentName.length <= Application.ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at ${settings.restApi.bindAddress.toString}")

    implicit val mat: ActorMaterializer = ActorMaterializer()
    val bindAddress = settings.restApi.bindAddress

    Http().bindAndHandle(httpService.compositeRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    })
  }

  private def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    upnpGateway.foreach(_.deletePort(settings.network.bindAddress.getPort))
    networkControllerRef ! ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>
      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

  private def isEmptyState: Boolean = {
    val dir = ErgoState.stateDir(ergoSettings)
    Option(dir.listFiles()).fold(true)(_.isEmpty)
  }

}

object ErgoApp extends ScorexLogging {
  Kamon.init()

  import com.joefkelley.argyle._

  val argParser: Arg[Args] = (
    optional[String]("--config", "-c") and
      optionalOneOf[NetworkType](NetworkType.all.map(x => s"--${x.verboseName}" -> x): _*)
    ).to[Args]

  def main(args: Array[String]): Unit = argParser.parse(args) match {
    case Success(argsParsed) => new ErgoApp(argsParsed).run()
    case Failure(e) => throw e
  }

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    log.warn("Terminating Actors")
    actors.foreach { a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }

}
