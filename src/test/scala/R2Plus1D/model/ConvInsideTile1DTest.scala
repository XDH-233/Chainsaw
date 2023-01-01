package R2Plus1D.model

class ConvInsideTile1DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work without err" in {
    val config = ConvInsideTile1DConfig(Uic = 2, Uc = 4, Uoc = 2, Tc = 5, Noc = 3, Nid = 5, Nhw = 2, Nod = 3, Kt = 3, stride = 2, padding = 1)
    val conv1D = ConvInsideTile1D(config)
    val ifMap  = conv1D.randomIfMap
    val weight = conv1D.randomWeight

    ifMap.foreach { id =>
      println("----------ic--------------")
      id.foreach { ih =>
        println("-----------id-------------")
        ih.foreach(w => println(w.mkString(" ")))
      }
    }
    weight.foreach { oc =>
      println("-------------oc-------------")
      oc.foreach { ic =>
        println(ic.mkString(" "))
      }
    }
    val ofMapTile = conv1D.loopUnroll(conv1D.ifMap2Tile(ifMap), conv1D.weight2Tile(weight))

    println("------------loop unroll result--------------------")
    ofMapTile.foreach(o => println(o.mkString(" ")))

    val ofMap            = conv1D.loop(ifMap, weight)
    val ofMapTileLoopCov = conv1D.ofMap2Tile(ofMap)
    println("--------------loop result ------------------------")
    ofMapTileLoopCov.foreach(o => println(o.mkString(" ")))

    ofMapTile.zip(ofMapTileLoopCov).foreach { case (u, l) => u.zip(l).foreach { case (a, b) => assert(a == b) } }
    println("loop result equal loop unroll result! test pass!")
  }

}
