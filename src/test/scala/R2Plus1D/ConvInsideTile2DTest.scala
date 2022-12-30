package R2Plus1D

class ConvInsideTile2DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work normally" in { // attention: it will consume about 6 minutes
    val config: ConvInsideTile2DConfig =
      ConvInsideTile2DConfig(Uic = 56, Uc = 128, Tic = 64, Tc = 144, Nid = 15, Nihw = 56, Nod = 15, Nohw = 56, Krs = 3, stride = 1, padding = 1)
    val conv2D = ConvInsideTile2D(config)

    val ifMap  = conv2D.randIfMap()
    val weight = conv2D.randWeight()

    // ifMap.foreach { ic =>
    //   println("-------c----------------------")
    //   ic.foreach(h => h.foreach(w => println(w.mkString(" "))))
    // }

    // weight.foreach { oc =>
    //   println("---------------oc-------------")
    //   oc.foreach { ic =>
    //     println("*********ic********** ")
    //     ic.foreach(kr => println(kr.mkString(" ")))
    //   }
    // }
    val loopUnrollRes = conv2D.loopUnroll(conv2D.ifMap2Tile(ifMap), conv2D.weight2Tile(weight))
    val loopRes       = conv2D.loop(ifMap, weight)
    val loopRes2Tile  = conv2D.ofMap2Tile(loopRes)

    // println("-------------------------unroll result-------------------------------------------")
    // loopUnrollRes.foreach(o => println(o.mkString(" ")))
    // println("---------------------------roll result-------------------------------------------")
    // loopRes2Tile.foreach(o => println(o.mkString(" ")))

    loopUnrollRes.zip(loopRes2Tile).foreach { case (u, l) => u.zip(l).foreach { case (uic, lic) => assert(uic == lic) } }

  }

}
