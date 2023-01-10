package R2Plus1D.model
import math.{ceil, sqrt}
import util.Random.nextInt

case class Conv2D(config: Conv2DConfig) {
  import config._

  private val ifMapSize:  Int = Nd * Nihw * Nihw
  private val kernelSize: Int = Krs * Krs
  private val ofMapSize:  Int = Nd * Nohw * Nohw
  private val NowhTile = sqrt(Uc / 2).toInt

  def randIfMap():  Array[Array[Array[Array[Int]]]] = Array.fill(Nic)(Array.fill(Nihw)(Array.fill(Nihw)(Array.fill(Nd)(nextInt(10)))))
  def randWeight(): Array[Array[Array[Array[Int]]]] = Array.fill(Nc)(Array.fill(Nic)(Array.fill(Krs)(Array.fill(Krs)(nextInt(10)))))

  def loopUnroll(ifMapTile: Array[Array[Int]], weightTile: Array[Array[Int]]): Array[Array[Int]] = {
    val ofMapTile = Array.fill(divideCeil(Nc, Uc) * ofMapSize)(Array.fill(Uc)(0))
    for (to <- 0 until divideCeil(Nc, Uc); ti <- 0 until divideCeil(Nic, Uic)) {
      for (th <- 0 until divideCeil(Nohw, NowhTile); tw <- 0 until divideCeil(Nihw, NowhTile)) {
        for (kr <- 0 until Krs; ks <- 0 until Krs) {
          for (sth <- 0 until NowhTile) {
            if (th * NowhTile + sth < Nohw) {
              for (stw <- 0 until NowhTile) {
                if (tw * NowhTile + stw < Nohw) {
                  for (od <- 0 until Nd) {
                    val ox = th * NowhTile + sth
                    val oy = tw * NowhTile + stw
                    val ix = ox * stride + kr - padding
                    val iy = oy * stride + ks - padding

                    val ifAddr:         Int               = ifMapAddr(ti, th, tw, kr, ks, sth, stw, od)
                    val ifMap:          Array[Int]        = if (ix < 0 || ix >= Nihw || iy < 0 || iy >= Nihw) Array.fill(Uic)(0) else ifMapTile(ifAddr)
                    val weightAddrHead: Int               = weightAddr(to, ti, kr, ks)
                    val weight:         Array[Array[Int]] = Array.tabulate(Uc)(o => weightTile(weightAddrHead + o)).reverse
                    val psum:           Array[Int]        = PEArray(ifMap, weight)
                    val ofAddr = ofMapAddr(to, th, tw, sth, stw, od)
                    psum.zipWithIndex.foreach { case (p, i) => ofMapTile(ofAddr)(i) += p }

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
    val ret = Array.fill(divideCeil(Nic, Uic) * ifMapSize)(Array.fill(Uic)(0))
    for (ic <- 0 until Nic; ih <- 0 until Nihw; iw <- 0 until Nihw; id <- 0 until Nd) {
      ret((ic / Uic) * ifMapSize + ih * Nihw * Nd + iw * Nd + id)(ic % Uic) = ifMap(ic)(ih)(iw)(id)
    }
    ret
  }

  def weight2Tile(weight: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = { // oc -> ic -> r -> s
    val ret = Array.fill(divideCeil(Nic, Uic) * divideCeil(Nc, Uc) * kernelSize * Uc)(Array.fill(Uic)(0))
    for (oc <- 0 until Nc; ic <- 0 until Nic; kr <- 0 until Krs; ks <- 0 until Krs) {
      ret((oc / Uc) * divideCeil(Nic, Uic) * kernelSize * Uc + (ic / Uic) * kernelSize * Uc + kr * Uc * Krs + ks * Uc + (Uc - 1 - oc % Uc))(
        ic % Uic
      ) = weight(oc)(ic)(kr)(ks) // Uc weight need to reversed
    }
    ret
  }

  def ofMap2Tile(ofMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Nc, Uc) * ofMapSize)(Array.fill(Uc)(0))
    for (oc <- 0 until Nc; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nd) {
      ret((oc / Uc) * ofMapSize + oh * Nohw * Nd + ow * Nd + od)(oc % Uc) = ofMap(oc)(oh)(ow)(od)
    }
    ret
  }

  def loop(ifMap: Array[Array[Array[Array[Int]]]], weight: Array[Array[Array[Array[Int]]]]): Array[Array[Array[Array[Int]]]] = { // ifMap: ic -> ih -> iw -> id; weight: oc -> ic -> r -> s
    require(ifMap.length  == Nic && ifMap.head.length == Nihw && ifMap.head.head.length == Nihw && ifMap.head.head.head.length == Nd, "ifMap size err")
    require(weight.length == Nc && weight.head.length == Nic && weight.head.head.length == Krs && weight.head.head.head.length == Krs, "weight size err")

    val ofMap = Array.fill(Nc)(Array.fill(Nohw)(Array.fill(Nohw)(Array.fill(Nd)(0)))) // oc -> oh -> ow -> od

    for (oc <- 0 until Nc; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nd) {
      var tmp = 0
      for (kr <- 0 until Krs; ks <- 0 until Krs) {
        for (ic <- 0 until Nic) {
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
    to * divideCeil(Nic, Uic) * Uc * kernelSize + ti * Uc * kernelSize + kr * Krs * Uc + ks * Uc

  private def ifMapAddr(ti: Int, th: Int, tw: Int, kr: Int, ks: Int, sth: Int, stw: Int, od: Int): Int =
    ti * ifMapSize +
      th * stride * Nihw * Nd * NowhTile +
      tw * NowhTile * stride * Nd +
      kr * Nihw * Nd + ks * Nd +
      sth * stride * Nihw * Nd +
      stw * stride * Nd +
      od - padding * Nihw * Nd - padding * Nd

  private def ofMapAddr(to: Int, th: Int, tw: Int, sth: Int, stw: Int, od: Int): Int =
    to * ofMapSize +
      th * NowhTile * Nohw * Nd +
      tw * NowhTile * Nd +
      sth * Nohw * Nd +
      stw * Nd +
      od

}
