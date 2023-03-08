package R2Plus1D.model

class Conv1DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work without err" in {
    val config = ConvConfig(Uic = 2, Uoc = 2, Tc = 4, Nic = 5, Noc = 5, Nid = 5, Nihw = 2, K = 3, stride = 2, padding = 1, convType = ConvType.D1)
    val conv1D = Conv1D(config)
    config.display()

    val ifMap  = conv1D.randomIfMap
    val weight = conv1D.randomWeight

//    ifMap.foreach { id =>
//      println("----------ic--------------")
//      id.foreach { ih =>
//        println("-----------id-------------")
//        ih.foreach(w => println(w.mkString(" ")))
//      }
//    }
//    weight.foreach { oc =>
//      println("-------------oc-------------")
//      oc.foreach { ic =>
//        println(ic.mkString(" "))
//      }
//    }
    val ofMapTile = conv1D.loopUnroll(conv1D.ifMap2Mem(ifMap), conv1D.weight2Mem(weight))

    println("------------loop unroll result--------------------")
    ofMapTile.foreach(o => println(o.mkString(" ")))

    val ofMap            = conv1D.loop(ifMap, weight)
    val ofMapTileLoopCov = conv1D.ofMap2Mem(ofMap)
    println("--------------loop result ------------------------")
    ofMapTileLoopCov.foreach(o => println(o.mkString(" ")))

    ofMapTile.zip(ofMapTileLoopCov).foreach { case (u, l) => u.zip(l).foreach { case (a, b) => assert(a == b) } }
    println("loop result equal loop unroll result! test pass!")
  }

}
