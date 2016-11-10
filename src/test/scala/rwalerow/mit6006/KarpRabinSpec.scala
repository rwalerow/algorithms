package rwalerow.mit6006

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by robert on 09.11.16.
  */
class KarpRabinSpec extends FlatSpec with Matchers {

  val loremIpsum = """lorem ipsum dolor sit amet, consectetur adipiscing elit.
                     aliquam vitae venenatis tellus. vestibulum dui urna, hendrerit eu sapien eu, rhoncus ornare sapien.
                     nunc scelerisque, ante sed dignissim placerat, sem neque molestie odio, eget sodales dui felis in magna.
                     duis mi justo, finibus id nisi ut, tristique tincidunt orci. mauris non lacus aliquam, placerat augue id, semper urna.
                     sed non nibh purus. maecenas pharetra, erat nec consequat ultricies, erat massa sagittis tellus, et aliquam felis leo id mauris.
                     integer vehicula diam et lobortis luctus.
                     vestibulum felis neque, luctus nec dictum vitae, commodo ac risus.
                     morbi ligula mauris, auctor sit amet nibh ac, commodo tempus leo.
                     etiam enim eros, pellentesque porta est id, semper varius justo.
                     integer sed vehicula nunc. mauris ullamcorper velit in diam aliquam, ac j enim faucibus.
                     phasellus consectetur faucibus lorem eu venenatis. Fusce ornare vehicula leo, sed lacinia nisi elementum quis."""

  "Karp-Rabin" should "find first word" in {
    KarpRabin.contains("lorem", loremIpsum) shouldBe true
  }

  it should "find moved word" in {
    KarpRabin.contains("orem", loremIpsum) shouldBe true
  }

  it should "find word in end" in {
    KarpRabin.contains("tristique", loremIpsum) shouldBe true
  }

  it should "find word later in string" in {
    val str = "hello i am a little kitty and i love milk"

    KarpRabin.contains("love", str) shouldBe true
  }

  it should "find short string" in {
    val strr = "ala, ma kota"
    KarpRabin.contains("ma", strr) shouldBe true
  }

  "Hash" should "correctly calculate" in {
    val basic = 127

    val result = KarpRabin.hash("ro", basic)

    result shouldBe 14589
  }

  "Hash/Slide" should "work in balance" in {
    val p = 997
    val basic = 127
    val magic = Math.pow(basic, 2).toInt
    val initHash = KarpRabin.hash("ro", basic)
    val movedReady = KarpRabin.hash("ob", basic)
    val movedReady2 = KarpRabin.hash("be", basic)

    val moved = KarpRabin.slide('b'.toByte, 'r'.toByte, 2, basic, initHash, magic)
    val moved2 = KarpRabin.slide('e'.toByte, 'o'.toByte, 2, basic, moved, magic)

    moved shouldBe movedReady
    moved2 shouldBe movedReady2
  }

  it should "work for whole lorem ipsum" in {
    val byteBase = 127
    val size = 4
    var slideHash = BigInt(0)
    val magic = BigInt(byteBase).pow(size)

    for(i <- 0 until loremIpsum.length - size) {
      if(i <= 0) slideHash = KarpRabin.hash(loremIpsum.substring(0, size), byteBase)
      else slideHash = KarpRabin.slide(loremIpsum(i + size - 1).toByte, loremIpsum(i - 1).toByte, size, byteBase, slideHash, magic)

      val countedHash = KarpRabin.hash(loremIpsum.substring(i, i + size), byteBase)
      countedHash shouldBe slideHash
    }
  }
}
