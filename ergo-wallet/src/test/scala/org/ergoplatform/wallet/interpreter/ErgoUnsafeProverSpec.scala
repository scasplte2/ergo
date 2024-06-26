package org.ergoplatform.wallet.interpreter

import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.wallet.crypto.ErgoSignature
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.util.Random

class ErgoUnsafeProverSpec
  extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with InterpreterSpecCommon {
  import org.ergoplatform.wallet.utils.WalletGenerators._


  it should "produce the same proof as a fully-functional prover" in {
    val entropy = Random.randomBytes(32)
    val extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy, usePre1627KeyDerivation = false)
    val fullProver = ErgoProvingInterpreter(extendedSecretKey, parameters)
    val unsafeProver = ErgoUnsafeProver

    forAll(unsignedTxGen(extendedSecretKey)) { case (ins, unsignedTx) =>

      val signedTxFull = fullProver.sign(unsignedTx, ins.toIndexedSeq, IndexedSeq(),
        stateContext, TransactionHintsBag.empty).get

      val signedTxUnsafe = unsafeProver.prove(unsignedTx, extendedSecretKey.privateInput)

      signedTxFull shouldEqual signedTxUnsafe

      signedTxFull.inputs.map(_.spendingProof.proof).zip(signedTxFull.inputs.map(_.spendingProof.proof))
        .foreach { case (fullProof, unsafeProof) =>
          ErgoSignature.verify(unsignedTx.messageToSign, fullProof, extendedSecretKey.publicKey.key.value) shouldBe
            ErgoSignature.verify(unsignedTx.messageToSign, unsafeProof, extendedSecretKey.publicKey.key.value)
        }
    }
  }

}
