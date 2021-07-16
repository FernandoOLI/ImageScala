package imageTransform

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.Array.ofDim

object ImageMatrix {

  def photoTomatrix(img: BufferedImage): Array[Array[Array[Int]]] = {
    val w = img.getWidth
    val h = img.getHeight

    val matrixRGBA = ofDim[Int](h, w, 4)
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

  def reduceImageMatrix(matrixRGBA: Array[Array[Array[Int]]]) = {
    val tupleMedida = escalaReducao(matrixRGBA(0).size, matrixRGBA.size)
    val w = tupleMedida._1
    val h = tupleMedida._2

    val w_normal = matrixRGBA(0).size
    val h_normal = matrixRGBA.size

    println("Width " ,matrixRGBA(0).size )
    println("Height " ,matrixRGBA.size )
    val rpImage = new BufferedImage(matrixRGBA(0).size, matrixRGBA.size, BufferedImage.TYPE_INT_RGB)
    createImage(0,0,matrixRGBA)

    def createImage(w1: Integer, h1: Integer, img: Array[Array[Array[Int]]]): Unit = {
      val r = matrixRGBA(h1)(w1)(1) * 65536
      val g = matrixRGBA(h1)(w1)(2) * 256
      val b = matrixRGBA(h1)(w1)(3)

      rpImage.setRGB(w1,h1, r+ g + b)

      if (h1 < h_normal - 1) {
        createImage(w1, h1 + 1, img)
      } else if (h1 == h_normal - 1 && w1 < w_normal - 1)
        createImage(w1 + 1, 0, img)
    }

    val resized = rpImage.getScaledInstance(w, h, Image.SCALE_DEFAULT)

    // Saving Image back to disck
    val bufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    bufferedImage.getGraphics.drawImage(resized, 0, 0, null)
    ImageIO.write(bufferedImage, "JPG", new File("src/main/resources/images/resized.jpg"))
    photoTomatrix(bufferedImage)
  }

  def saveGrayImage(imageGray: Array[Array[Double]], name: String) = {
    val w = imageGray(0).size
    val h = imageGray.size
    var rp = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY)
    readImage(0, 0, imageGray)

    def readImage(w1: Integer, h1: Integer, img: Array[Array[Double]]): Unit = {
      rp.setRGB(w1, h1, imageGray(h1)(w1).toInt + 350)

      if (h1 < h - 1)
        readImage(w1, h1 + 1, img)
      else if (h1 == h - 1 && w1 < w - 1)
        readImage(w1 + 1, 0, img)
    }

    ImageIO.write(rp, "jpg", new File("src/main/resources/images/"+name+".jpg"))
  }

  /*
  * Convertendo a Matriz multidimensional para uma matriz de uma dimensão em escala de cinza
  *
  * */
  def toGray(matrixRGBA: Array[Array[Array[Int]]]): Array[Array[Double]] = {
    val w = matrixRGBA(0).size
    val h = matrixRGBA.size
    val matrixGray = ofDim[Double](h, w)
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

  /*
  * Função para reduzir ou aumentar o tamanho da imagem proporcionalmente
  * levando em conta tbm valores mínimos
  * */
  def escalaReducao(width: Integer, height: Integer): (Integer, Integer) = {

    if (width > Config.basewidth) {
      val size_height =( height.toDouble * (Config.basewidth.toDouble / width.toDouble))
      if (size_height <= Config.min_height || size_height.isNaN) {
        return (Config.basewidth, Config.min_height)
      } else
        return (Config.basewidth, size_height.toInt)
    }

    if (height > Config.baseheight) {
      val size_width = (width.toDouble * (Config.baseheight.toDouble / height.toDouble))
      if (size_width <= Config.min_height || size_width.isNaN) {
        (Config.basewidth, Config.min_height)
      }
      else {
        (Config.basewidth, size_width.toInt)
      }
    }
    else (width, height)
  }
}
