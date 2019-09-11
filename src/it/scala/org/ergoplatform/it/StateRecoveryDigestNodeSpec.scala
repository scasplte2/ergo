package org.ergoplatform.it

import java.io.File

import akka.japi.Option.Some
import com.typesafe.config.Config
import org.apache.commons.io.FileUtils
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class StateRecoveryDigestNodeSpec extends FreeSpec with IntegrationSuite {

  val approxMinerTargetHeight = 20
  val approxFollowerTargetHeight: Int = approxMinerTargetHeight + 5

  val minerLocalVolume = s"$localDataDir/state-recovery-spec/miner/data"
  val followerLocalVolume = s"$localDataDir/state-recovery-spec/follower/data"
  val remoteVolume = "/app"

  val dir = new File(minerLocalVolume)
  dir.mkdirs()

  val minerConfig: Config = nodeSeedConfigs.head
    .withFallback(miningDelayConfig(10000))
    .withFallback(specialDataDirConfig(remoteVolume))
  val followerConfig: Config = digestStateNodeConfig
    .withFallback(blockIntervalConfig(10000))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))
    .withFallback(specialDataDirConfig(remoteVolume))

  //  Testing scenario:
  // 1. Start up one node and let it mine {approxMinerTargetHeight} blocks;
  // 2. Shut it down and copy its history to testing node's directory;
  // 3. Start mining node again;
  // 4. Start testing node and wait until it gets synced with the mining node + {approxFollowerTargetHeight}
  //    - it would require testing node to recover state correctly and apply new blocks on top of it;
  "Startup with only history available" in {

    val minerNode: Node = docker.startNode(minerConfig, specialVolumeOpt = Some((minerLocalVolume, remoteVolume))).get

    val result = Async.async {
      Async.await(minerNode.waitForFullHeight(approxMinerTargetHeight))
      docker.stopNode(minerNode, secondsToWait = 0)

      FileUtils.copyDirectoryToDirectory(new File(s"$minerLocalVolume/history"), new File(followerLocalVolume))

      val nodeForSyncing: Node = docker
        .startNode(minerConfig, specialVolumeOpt = Some((minerLocalVolume, remoteVolume))).get
      Async.await(nodeForSyncing.waitForFullHeight(approxMinerTargetHeight + 2))

      val followerNode: Node = docker
        .startNode(followerConfig, specialVolumeOpt = Some((followerLocalVolume, remoteVolume))).get
      Async.await(followerNode.waitForFullHeight(approxFollowerTargetHeight))
    }

    Await.result(result, 10.minutes)
  }

}
