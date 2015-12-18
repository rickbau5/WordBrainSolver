package com.bau5.wordbrainsolver

import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}


/**
  * Created by Rick on 12/10/15.
  */
class ImageReaderSpec extends WordSpec with Matchers with BeforeAndAfterAll {
  var reader: ImageReader = _
  val config = ConfigFactory.load()

  "The image reader" should {

    "have the correct color for the background" in {
      val img = reader.loadImageAtPath(getImagePath("3x3"))
      (config.getString("image-reader.colors.background")
        should be (Integer.toHexString(img.getRGB(150, 150))))
    }

    var props3 = Option.empty[BoardProperties]
    "load the properties for the 3x3" in {
      props3 = Option(reader.processScreenshot(config, getImagePath("3x3")))
      props3.isDefined should be (true)
    }
    "find the right number of tiles in the 3x3" in {
      props3.get.getTotalTiles should be (9)
    }
    "find the right characters in the 3x3" in {
      props3.get.getOutput should be ("d t r\ni a e\ne c h\n\n")
    }
    "find the right number and lengths of words in the 3x3" in {
      props3.get.boxes should have length 2
      props3.get.boxes should contain allOf (5, 4)
    }

    var props4 = Option.empty[BoardProperties]
    "load the properties for the 4x4" in {
      props4 = Option(reader.processScreenshot(config, getImagePath("4x4")))
      props4.isDefined should be (true)
    }
    "find the right number of tiles in the 4x4" in {
      props4.get.getTotalTiles should be (16)
    }
    "find the right characters in the 4x4" in {
      props4.get.getOutput should be ("d p e y\nr c a i\nr o n c\ne c h m\n\n")
    }
    "find the right number and lengths of words in the 4x4" in {
      props4.get.boxes should have length 3
      props4.get.boxes should contain allOf (3, 6, 7)
    }
  }


  override protected def beforeAll(): Unit = {
    reader = new ImageReader
    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    reader.post()
    super.afterAll()
  }

  def getImagePath(str: String): String = {
    config.getString("solver.input-path") + str + ".png"
  }
}
