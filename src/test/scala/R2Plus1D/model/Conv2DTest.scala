package R2Plus1D.model

class Conv2DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work normally" in { // attention: it will consume about 6 minutes
    val config: ConvConfig =
      ConvConfig(Uic = 4, Uoc = 8, Nic = 9, Tc = 16, Noc = 18, Nid = 2, Nihw = 4, K = 2, stride = 1, padding = 0, convType = ConvType.D2)
    val conv2D = Conv2D(config)
    config.display()
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
    val loopUnrollRes = conv2D.loopUnroll(conv2D.ifMap2Mem(ifMap), conv2D.weight2Mem(weight), printProcession = true)
    val loopRes       = conv2D.loop(ifMap, weight)
    val loopRes2Tile  = conv2D.ofMap2Mem(loopRes)

    println("-------------------------unroll result-------------------------------------------")
    loopUnrollRes.foreach(o => println(o.mkString(" ")))
    println("---------------------------roll result-------------------------------------------")
    loopRes2Tile.foreach(o => println(o.mkString(" ")))

    loopUnrollRes.zip(loopRes2Tile).foreach { case (u, l) => u.zip(l).foreach { case (uic, lic) => assert(uic == lic) } }

  }

}
