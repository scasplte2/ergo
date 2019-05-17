package org.ergoplatform.settings

import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.util.serialization.{Reader, Writer}

case class SoftForkParameters(startingHeight: Height, votesCollected: Int, rulesDisabled: Seq[Short]) extends BytesSerializable {
  override type M = SoftForkParameters

  override def serializer: ScorexSerializer[SoftForkParameters] = SoftForkParametersSerializer
}

object SoftForkParametersSerializer extends ScorexSerializer[SoftForkParameters] {
  override def serialize(obj: SoftForkParameters, w: Writer): Unit = {
    w.putInt(obj.startingHeight)
    w.putInt(obj.votesCollected)
    w.putInt(obj.rulesDisabled.length)
    obj.rulesDisabled.foreach{ r =>
      w.putShort(r)
    }
  }

  override def parse(r: Reader): SoftForkParameters = {
    val startingHeight = r.getInt()
    val votesCollected = r.getInt()
    val rulesLength = r.getInt()
    val rulesDisabled = (0 until rulesLength) map (_ => r.getShort())
    SoftForkParameters(startingHeight, votesCollected, rulesDisabled)
  }
}