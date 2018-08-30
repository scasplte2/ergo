package org.ergoplatform.nodeView.wallet

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ChainStatus.{Offchain, Onchain}
import org.ergoplatform.nodeView.wallet.SpendingStatus.{Spent, Unspent}
import org.ergoplatform.utils.{ErgoPropertyTest, WalletGenerators}
import org.scalacheck.Gen

class BoxTransitionSpec extends ErgoPropertyTest with WalletGenerators {

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 10)

  def heightGen: Gen[Int] = Gen.choose(0, Integer.MAX_VALUE)
  def spendingTxGen: Gen[ErgoTransaction] = invalidErgoTransactionGen

  property("Creation Confirmation from Unspent Off-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOffchainBoxGen, heightGen) { (box, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, CreationConfirmation(height))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Unspent
      transited.chainStatus shouldBe Onchain
      transited shouldBe a[UnspentOnchainBox]
      transited.asInstanceOf[UnspentOnchainBox].creationHeight shouldBe height
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Creation Confirmation from Spent Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentOffchainBoxGen, heightGen) { (box, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, CreationConfirmation(height))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Spent
      transited.chainStatus shouldBe Offchain
      transited shouldBe a[SpentOffchainBox]
      transited.asInstanceOf[SpentOffchainBox].creationHeightOpt shouldBe Some(height)
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Creation Confirmation from Spent partially Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentPartiallyOffchainBoxGen, heightGen) { (box, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, CreationConfirmation(height))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Creation Confirmation from Unspent On-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOnchainBoxGen, heightGen) { (box, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, CreationConfirmation(height))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Creation Confirmation from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen, heightGen) { (box, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, CreationConfirmation(height))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process off-chain Spending from Unspent Off-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOffchainBoxGen, spendingTxGen) { (box, tx) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, None))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Spent
      transited.chainStatus shouldBe Offchain
      transited shouldBe a[SpentOffchainBox]
      transited.asInstanceOf[SpentOffchainBox].creationHeightOpt shouldBe None
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process off-chain Spending from Spent Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentOffchainBoxGen, spendingTxGen) { (box, tx) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, None))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process off-chain Spending from Spent partially Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentPartiallyOffchainBoxGen, spendingTxGen) { (box, tx) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, None))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process off-chain Spending from Unspent On-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOnchainBoxGen, spendingTxGen) { (box, tx) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, None))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Spent
      transited.chainStatus shouldBe Offchain
      transited shouldBe a[SpentOffchainBox]
      transited.asInstanceOf[SpentOffchainBox].creationHeightOpt shouldBe Some(box.creationHeight)
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process off-chain Spending from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen, spendingTxGen) { (box, tx) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, None))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process on-chain Spending from Unspent Off-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOffchainBoxGen, spendingTxGen, heightGen) { (box, tx, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, Some(height)))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process on-chain Spending from Spent Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentOffchainBoxGen, spendingTxGen, heightGen) { (box, tx, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, Some(height)))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process on-chain Spending from Spent partially Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentPartiallyOffchainBoxGen, spendingTxGen, heightGen) { (box, tx, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, Some(height)))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Spent
      transited.chainStatus shouldBe Onchain
      transited shouldBe a[SpentOnchainBox]
      transited.asInstanceOf[SpentOnchainBox].creationHeight shouldBe box.creationHeightOpt.value
      transited.asInstanceOf[SpentOnchainBox].spendingHeight shouldBe height
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process on-chain Spending from Unspent On-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOnchainBoxGen, spendingTxGen, heightGen) { (box, tx, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, Some(height)))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Spent
      transited.chainStatus shouldBe Onchain
      transited shouldBe a[SpentOnchainBox]
      transited.asInstanceOf[SpentOnchainBox].creationHeight shouldBe box.creationHeight
      transited.asInstanceOf[SpentOnchainBox].spendingHeight shouldBe height
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process on-chain Spending from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen, spendingTxGen, heightGen) { (box, tx, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessSpending(tx, Some(height)))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback from Unspent Off-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOffchainBoxGen, heightGen) { (box, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback from Spent Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentOffchainBoxGen, heightGen) { (box, height) =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback from Spent partially Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentPartiallyOffchainBoxGen) { box =>
      val height = Gen.choose(0, box.creationHeightOpt.value - 1).sample.value
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Spent
      transited.chainStatus shouldBe Offchain
      transited shouldBe a[SpentOffchainBox]
      transited.asInstanceOf[SpentOffchainBox].creationHeightOpt shouldBe None
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process creation-height Rollback from Spent partially Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentPartiallyOffchainBoxGen) { box =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(box.creationHeightOpt.value))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback to higher-than-creation height from Spent partially Off-chain Box") {
    val registry = new WalletStorage
    forAll(spentPartiallyOffchainBoxGen) { box =>
      val height = Gen.choose(box.creationHeightOpt.value + 1, Integer.MAX_VALUE).sample.value
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback from Unspent On-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOnchainBoxGen) { box =>
      val height = Gen.choose(0, box.creationHeight - 1).sample.value
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Unspent
      transited.chainStatus shouldBe Offchain
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process creation-height Rollback from Unspent On-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOnchainBoxGen) { box =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(box.creationHeight))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback higher-than-creation height from Unspent On-chain Box") {
    val registry = new WalletStorage
    forAll(unspentOnchainBoxGen) { box =>
      val height = Gen.choose(box.creationHeight, Integer.MAX_VALUE).sample.value
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen) { box =>
      val height = Gen.choose(0, box.creationHeight).sample.value
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Unspent
      transited.chainStatus shouldBe Offchain
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process creation-height Rollback from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen) { box =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(box.creationHeight))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Unspent
      transited.chainStatus shouldBe Onchain
      transited shouldBe a[UnspentOnchainBox]
      transited.asInstanceOf[UnspentOnchainBox].creationHeight shouldBe box.creationHeight
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process Rollback to higher-than-creation-but-lower-than-spending height from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen) { box =>
      val height = Gen.choose(box.creationHeight + 1, box.spendingHeight - 1).sample.value
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(height))
      registry.byId(box.boxId) should not be empty
      val transited = registry.byId(box.boxId).value
      transited.spendingStatus shouldBe Unspent
      transited.chainStatus shouldBe Onchain
      transited shouldBe a[UnspentOnchainBox]
      transited.asInstanceOf[UnspentOnchainBox].creationHeight shouldBe box.creationHeight
      transited.certainty shouldBe box.certainty
      transited.value shouldBe box.value
    }
  }

  property("Process spending-height Rollback from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen) { box =>
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(box.spendingHeight))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }

  property("Process Rollback to higher-than-spending height from Spent On-chain Box") {
    val registry = new WalletStorage
    forAll(spentOnchainBoxGen) { box =>
      val height = Gen.choose(box.spendingHeight + 1, Integer.MAX_VALUE).sample.value
      registry.put(box)
      registry.makeTransition(box.boxId, ProcessRollback(box.spendingHeight))
      registry.byId(box.boxId) should not be empty
      val same = registry.byId(box.boxId).value
      same shouldBe box
    }
  }
}
