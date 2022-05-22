package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals
import scala.util.Random

class BlurSuite {
  def getRandomPixelArray(width: Int, height: Int): Array[Int] =
    Seq.fill(width * height)(Random.between(0, Int.MaxValue)).toArray
  val arr3x3: Array[RGBA] = getRandomPixelArray(3, 3)
  val arr32x32: Array[RGBA] = getRandomPixelArray(32, 32)

  var tasks, radius = 4
  val img3x3 = new Img(3, 3, arr3x3)
  val dst3x3 = new Img(3, 3, arr3x3.clone)
  val img3x3_cp1 = new Img(3, 3, arr3x3.clone)
  val dst3x3_cp1 = new Img(3, 3, arr3x3.clone)

  val img32x32 = new Img(32, 32, arr32x32)
  val dst32x32 = new Img(32, 32, arr32x32.clone)
  val img32x32_cp1 = new Img(32, 32, arr32x32.clone)
  val dst32x32_cp1 = new Img(32, 32, arr32x32.clone)

  val img3x4 = new Img(3, 4, getRandomPixelArray(3, 4))

  def isImgNotEqual(img1: Img, img2: Img):Boolean = {
    for( x <- 0 until img1.width; y <- 0 until img2.height)
      if (img1(x,y) == img2(x,y)) return false
    true
  }

  @Test def `Blur should modify each pixel of 32x32 image`(): Unit = {
    HorizontalBoxBlur.parBlur(img32x32, dst32x32, tasks, radius)
    assert(isImgNotEqual(img32x32, dst32x32))
    VerticalBoxBlur.parBlur(img32x32_cp1, dst32x32_cp1, tasks, radius)
    assert(isImgNotEqual(img32x32_cp1, dst32x32_cp1))
  }

  @Test def `Correctly blur 3x3 image`(): Unit = {
    radius = 1
    tasks = 4
    HorizontalBoxBlur.parBlur(img3x3, dst3x3, tasks, radius)
    assert(isImgNotEqual(img3x3, dst3x3))
    VerticalBoxBlur.parBlur(img3x3_cp1, dst3x3_cp1, tasks, radius)
    assert(isImgNotEqual(img3x3_cp1, dst3x3_cp1))
  }

  @Test def `BoxBlurKernel is correct`(): Unit = {
    radius = 1
    tasks = 2
    val rgbaComputed = boxBlurKernel(img3x4, 1, 2, radius)
    var r, g, b, a = 0
    for ( x <- 0 to 2; y <- 1 to 3)
    {
      val pixel = img3x4(x, y)
      r += red(pixel); g += green(pixel); b += blue(pixel); a += alpha(pixel)
    }
    assert( rgba(r/9, g/9, b/9, a/9) == rgbaComputed)
  }

  @Test def `Handle 0 radius correctly`(): Unit = {
    radius = 0
    tasks = 5
    val rgbaComputed = boxBlurKernel(img3x3, 0, 0, radius)
    assert(rgbaComputed == img3x3(0, 0))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

