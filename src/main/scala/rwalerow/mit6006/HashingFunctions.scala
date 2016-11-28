package rwalerow.mit6006

/**
  * Created by robert on 27.11.16.
  */
object HashingFunctions {

  def summingHash(str: String, p: Int = 97, mod: Int = 997): Int = {
    str.foldLeft(0)((acc, cha) => (cha.toByte % mod + ((acc * p) % mod)) % mod)
  }

  def hashWithIteration(str: String, i: Int = 0, p: Int = 97, mod: Int = 997): Int = {
    val baseHash = summingHash(str, p, mod)
    (baseHash + (i * baseHash) % mod) % mod
  }

  def hash(str: String, basic: Int): BigInt = {
    str.reverse.zipWithIndex.map{ case (ch, i) => ch.toByte * BigInt(basic).pow(i) }.sum
  }
}
