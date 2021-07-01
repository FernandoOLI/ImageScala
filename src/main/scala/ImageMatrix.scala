import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.Array.ofDim

object ImageMatrix extends App {
  val matrix = photoTomatrix(ImageIO.read(new File("src/main/resources/image.png")))
  val matrixGray = toGray(matrix)
  saveGrayImage(matrixGray)
  private def photoTomatrix(img: BufferedImage):Array[Array[Array[Int]]] = {
    val w = img.getWidth
    val h =  img.getHeight

    var matrixRGBA = ofDim[Int](h, w, 4)
    readImage(0, 0, img)

    def readImage(w1: Integer, h1: Integer, img: BufferedImage): Unit = {
      val rgb = img.getRGB(w1, h1)
      val a = rgb >> 32 & 0xff
      val r = rgb >> 16 & 0xff
      val g = rgb >> 8 & 0xff
      val b = rgb & 0xff

      matrixRGBA(h1)(w1)(0) = a;
      matrixRGBA(h1)(w1)(1) = r;
      matrixRGBA(h1)(w1)(2) = g;
      matrixRGBA(h1)(w1)(3) = b;
      if (h1 < h - 1) {
        readImage(w1, h1 + 1, img)
      } else if (h1 == h - 1 && w1 < w - 1)
        readImage(w1 + 1, 0, img)
    }
    matrixRGBA
  }

  def reduceImageMatrix(matrixRGBA: Array[Array[Array[Int]]]): Array[Array[Array[Int]]] = {
    println("teste")
    matrixRGBA
  }
  def saveGrayImage(imageGray: Array[Array[Double]]) = {

    val w = imageGray.size
    val h = imageGray(0).size
    var rp = new BufferedImage(h, w, BufferedImage.TYPE_BYTE_GRAY)
    readImage(0,0,imageGray)
    def readImage(w1: Integer, h1: Integer, img: Array[Array[Double]]): Unit = {
      rp.setRGB(h1,w1, imageGray(w1)(h1).toInt + 255)

      if (h1 < h - 1)
        readImage(w1, h1 + 1, img)
      else if (h1 == h - 1 && w1 < w - 1)
        readImage(w1 + 1, 0, img)
    }
    ImageIO.write(rp, "jpg", new File("src/main/resources/test.jpg"))
  }
  def toGray(matrixRGBA: Array[Array[Array[Int]]]): Array[Array[Double]] = {
    val w = matrixRGBA.size
    val h = matrixRGBA.length
    var matrixGray = ofDim[Double](w, h)
    readImage(0, 0, matrixRGBA)

    def readImage(w1: Integer, h1: Integer, img: Array[Array[Array[Int]]]): Unit = {
      matrixGray(h1)(w1) = 0.2989 * img(h1)(w1)(1) + 0.5870 * img(h1)(w1)(2) + 0.1140 * img(h1)(w1)(3)
      if (h1 < h - 1)
        readImage(w1, h1 + 1, img)
      else if (h1 == h - 1 && w1 < w - 1)
        readImage(w1 + 1, 0, img)
    }
    matrixGray
  }
}