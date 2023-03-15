package R2Plus1D.model

class Conv0DTest extends org.scalatest.flatspec.AnyFlatSpec {

  it should "work right" in {
    val config = ConvConfig(Uic = 2, Nic = 3, Uoc = 2, Noc = 5, Tc = 4, Nihw = 4, Nid = 2, K = 1, stride = 2, padding = 0, convType = ConvType.D0)
    config.display()
    val conv0D       = Conv0D(config)
    val ifMap        = conv0D.randomIfMap
    val weight       = conv0D.randomWeight
    val ofMap        = conv0D.loop(ifMap, weight)
    val ofMapUnroll  = conv0D.loopUnroll(conv0D.ifMap2Mem(ifMap), conv0D.weight2Mem(weight))
    val ofMapRollMem = conv0D.ofMap2Mem(ofMap)
    println("Unroll result")
    ofMapUnroll.foreach(o => println(o.mkString(" ")))
    println("loop roll result")
    ofMapRollMem.foreach(o => println(o.mkString(" ")))
    ofMapUnroll.zip(ofMapRollMem).foreach { case (u, r) => u.zip(r).foreach { case (a, b) => assert(a == b) } }
  }

}
