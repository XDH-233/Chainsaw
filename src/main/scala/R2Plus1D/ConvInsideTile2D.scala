package R2Plus1D
import math.{abs, ceil, sqrt}
import util.Random.nextInt
import util.control.Breaks

case class ConvInsideTile2D(config: ConvInsideTile2DConfig) {
  import config._

  private val ifMapSize:  Int = Nid * Nihw * Nihw
  private val kernelSize: Int = Krs * Krs
  private val ofMapSize:  Int = Nod * Nohw * Nohw
  private val NowhTile = sqrt(Uc / 2).toInt

  def randIfMap():  Array[Array[Array[Array[Int]]]] = Array.fill(Tic)(Array.fill(Nihw)(Array.fill(Nihw)(Array.fill(Nid)(nextInt(10)))))
  def randWeight(): Array[Array[Array[Array[Int]]]] = Array.fill(Tc)(Array.fill(Tic)(Array.fill(Krs)(Array.fill(Krs)(nextInt(10)))))

  def loopUnroll(ifMapTile: Array[Array[Int]], weightTile: Array[Array[Int]]): Array[Array[Int]] = { // FIXME
    val ofMapTile = Array.fill(divideCeil(Tc, Uc) * ofMapSize)(Array.fill(Uc)(0))
    for (to <- 0 until divideCeil(Tc, Uc); ti <- 0 until divideCeil(Tic, Uic)) {
      for (th <- 0 until divideCeil(Nohw, NowhTile); tw <- 0 until divideCeil(Nihw, NowhTile)) {
        for (kr <- 0 until Krs; ks <- 0 until Krs) {
          for (sth <- 0 until NowhTile) {
            if (th * NowhTile + sth < Nohw) {
              for (stw <- 0 until NowhTile) {
                if (tw * NowhTile + stw < Nohw) {
                  for (od <- 0 until Nod) {
                    val ox = th * NowhTile + sth
                    val oy = tw * NowhTile + stw
                    val ix = ox * stride + kr - padding
                    val iy = oy * stride + ks - padding

                    val ifAddr:         Int               = ifMapAddr(ti, th, tw, kr, ks, sth, stw, od)
                    val ifMap:          Array[Int]        = if (ix < 0 || ix >= Nihw || iy < 0 || iy >= Nihw) Array.fill(Uic)(0) else ifMapTile(ifAddr)
                    val weightAddrHead: Int               = weightAddr(to, ti, kr, ks)
                    val weight:         Array[Array[Int]] = Array.tabulate(Uc)(o => weightTile(weightAddrHead + o))
                    val psum:           Array[Int]        = PE(ifMap, weight)
                    val ofAddr = ofMapAddr(to, th, tw, sth, stw, od)
                    // println(s"to:$to, ti:$ti, th:$th, tw:$tw, kr:$kr, ks:$ks, sth:$sth, stw:$stw, od:$od")
                    // println(s"ox:$ox, oy:$oy, ix:$ix, iy:$iy, ifAddr:$ifAddr, weightAddrHead:$weightAddrHead, ofAddr:$ofAddr")
                    // println("ifMap: " + ifMap.mkString(" "))
                    // println("weight")
                    // weight.foreach(uic => println(uic.mkString(" ")))
                    // println("psum: " + psum.mkString(" "))
                    psum.zipWithIndex.foreach { case (p, i) => ofMapTile(ofAddr)(i) += p }

                    println("")
                  }
                }
              }
            }

          }

        }
      }
    }
    ofMapTile
  }

  def ifMap2Tile(ifMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = { // ic -> ih -> iw -> id
    val ret = Array.fill(divideCeil(Tic, Uic) * ifMapSize)(Array.fill(Uic)(0))
    for (ic <- 0 until Tic; ih <- 0 until Nihw; iw <- 0 until Nihw; id <- 0 until Nid) {
      ret((ic / Uic) * ifMapSize + ih * Nihw * Nid + iw * Nid + id)(ic % Uic) = ifMap(ic)(ih)(iw)(id)
    }
    ret
  }

  def weight2Tile(weight: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = { // oc -> ic -> r -> s // FIXME
    val ret = Array.fill(divideCeil(Tic, Uic) * divideCeil(Tc, Uc) * kernelSize * Uc)(Array.fill(Uic)(0))
    for (oc <- 0 until Tc; ic <- 0 until Tic; kr <- 0 until Krs; ks <- 0 until Krs) {
      ret((oc / Uc) * divideCeil(Tic, Uic) * kernelSize * Uc + (ic / Uic) * kernelSize * Uc + kr * Uc * Krs + ks * Uc + oc % Uc)(ic % Uic) =
        weight(oc)(ic)(kr)(ks)
    }
    ret
  }

  def ofMap2Tile(ofMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Tc, Uc) * ofMapSize)(Array.fill(Uc)(0))
    for (oc <- 0 until Tc; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nod) {
      ret((oc / Uc) * ofMapSize + oh * Nohw * Nod + ow * Nod + od)(oc % Uc) = ofMap(oc)(oh)(ow)(od)
    }
    ret
  }

  def loop(ifMap: Array[Array[Array[Array[Int]]]], weight: Array[Array[Array[Array[Int]]]]): Array[Array[Array[Array[Int]]]] = { // ifMap: ic -> ih -> iw -> id; weight: oc -> ic -> r -> s
    require(ifMap.length  == Tic && ifMap.head.length == Nihw && ifMap.head.head.length == Nihw && ifMap.head.head.head.length == Nid, "ifMap size err")
    require(weight.length == Tc && weight.head.length == Tic && weight.head.head.length == Krs && weight.head.head.head.length == Krs, "weight size err")

    val ofMap = Array.fill(Tc)(Array.fill(Nohw)(Array.fill(Nohw)(Array.fill(Nod)(0)))) // oc -> oh -> ow -> od

    for (oc <- 0 until Tc; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nod) {
      var tmp = 0
      for (kr <- 0 until Krs; ks <- 0 until Krs) {
        for (ic <- 0 until Tic) {
          val ix         = oh * stride + kr - padding
          val iy         = ow * stride + ks - padding
          val ifMapPixel = if (ix < 0 || ix >= Nihw || iy < 0 || iy >= Nihw) 0 else ifMap(ic)(ix)(iy)(od)
          tmp += weight(oc)(ic)(kr)(ks) * ifMapPixel
        }
      }
      ofMap(oc)(oh)(ow)(od) = tmp
    }
    ofMap
  }

  private def divideCeil(a: Int, b: Int): Int = ceil(a.toDouble / b.toDouble).toInt

  private def weightAddr(to: Int, ti: Int, kr: Int, ks: Int): Int =
    to * divideCeil(Tic, Uic) * Uc * kernelSize + ti * Uc * kernelSize + kr * Krs * Uc + ks * Uc // FIXME

  private def ifMapAddr(ti: Int, th: Int, tw: Int, kr: Int, ks: Int, sth: Int, stw: Int, od: Int): Int =
    ti * ifMapSize +
      th * stride * Nihw * Nid * NowhTile +
      tw * NowhTile * stride * Nid +
      kr * Nihw * Nid + ks * Nid +
      sth * stride * Nihw * Nid +
      stw * stride * Nid +
      od - padding * Nihw * Nid - padding * Nid

  private def ofMapAddr(to: Int, th: Int, tw: Int, sth: Int, stw: Int, od: Int): Int =
    to * ofMapSize +
      th * NowhTile * Nohw * Nod +
      tw * NowhTile * Nod +
      sth * Nohw * Nod +
      stw * Nod +
      od

  private def PE(ifMap: Array[Int], weight: Array[Array[Int]]): Array[Int] = weight.map(W => W.zip(ifMap).map { case (w, i) => w * i }.sum)
}
