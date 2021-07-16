package imageTransform


import imageTransform.ImageMatrix._

import java.io.File
import javax.imageio.ImageIO

object main extends App {

  val matrix = photoTomatrix(ImageIO.read(new File("src/main/resources/images/image.png")))
  val matrixGray = toGray(matrix)
  saveGrayImage(matrixGray, "grayImage")
  val matrixReduce = reduceImageMatrix(matrix)
  saveGrayImage(toGray(matrixReduce),"reduceImage")
}
