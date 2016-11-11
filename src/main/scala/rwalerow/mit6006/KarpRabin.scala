package rwalerow.mit6006

/**
  * Improvement proposal
  * Implement rolling hash as separate class
  */
object KarpRabin {

  val byteBase = 100

  def contains(str: String, in: String): Boolean = {

    val p = 997
    val magic = BigInt(byteBase).pow(str.length)
    val findHash = hash(str, byteBase) % p
    var searchHash = BigInt(0)

    for(i <- 0 until (in.length - str.length) ) {
      if(i <= 0) searchHash = hash(in.substring(0, str.length), byteBase)
      else searchHash = slide(in(i + str.length - 1).toByte, in(i - 1).toByte, str.length, byteBase, searchHash, magic)

      if(findHash == (searchHash % p) && str == in.substring(i, i + str.length)) return true
    }
    false
  }

  def hash(str: String, basic: Int): BigInt = {
    str.zipWithIndex.map{ case (ch, ind) => ch.toByte * BigInt(basic).pow(str.length - ind - 1) }.sum
  }

  def slide(
             nNumber: Byte,
             oNumber: Byte,
             size: Int,
             base: Int = byteBase,
             hashFullValue: BigInt,
             magic: BigInt): BigInt = {
    hashFullValue * base - oNumber * magic + BigInt(nNumber)
  }
}
