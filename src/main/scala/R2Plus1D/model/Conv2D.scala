package R2Plus1D.model
import math.{ceil, sqrt}
import scala.collection.mutable
import scala.language.postfixOps
import util.Random.nextInt

case class Conv2D(config: ConvConfig) {
  import config._
  val Nc = Noc
  val Uc = Uoc

  val loopAndAddr: mutable.Map[Loop, AddrIndex] = scala.collection.mutable.Map[Loop, AddrIndex]()

  def randIfMap():  Array[Array[Array[Array[Int]]]] = Array.fill(Nic)(Array.fill(Nihw)(Array.fill(Nihw)(Array.fill(Nid)(nextInt(10)))))
  def randWeight(): Array[Array[Array[Array[Int]]]] = Array.fill(Nc)(Array.fill(Nic)(Array.fill(K)(Array.fill(K)(nextInt(10)))))

  def loopUnroll(ifMapTile: Array[Array[Int]], weightMem: Array[Array[Array[Int]]], printProcession: Boolean = true): Array[Array[Int]] = {

    val ofMapTile = Array.fill(NocDUocCeil * ofMapSize)(Array.fill(Uc)(0))
    for (tc <- 0 until NocDTcCeil) {
      val weightTile = weightMem(tc)
      for (to <- 0 until TcDUocCeil) {
        if (tc * Tc + to * Uoc < NocDUocCeil * Uoc) { // layer done
          for (th <- Range(0, Nohw, Toh); tw <- Range(0, Nohw, Tow)) {
            for (ti <- 0 until NicDUicCeil) {
              for (kr <- 0 until K; ks <- 0 until K) {
                val weightAddrHead: Int = weightAddr(to, ti, kr, ks)
                for (sth <- 0 until Toh) {
                  for (stw <- 0 until Tow) {
                    for (od <- 0 until Nod) {
                      val ox = th + sth
                      val oy = tw + stw
                      val ix = ox * stride + kr - padding
                      val iy = oy * stride + ks - padding
                      val ifAddr: Int = ifMapAddr(ti, th, tw, kr, ks, sth, stw, od)
                      val ifMap: Array[Int] =
                        if (ix < 0 || ix >= Nihw || iy < 0 || iy >= Nihw || th + sth >= Nohw || tw + stw >= Nohw)
                          Array.fill(Uic)(0)
                        else
                          ifMapTile(ifAddr)

                      val weight: Array[Array[Int]] = Array.tabulate(Uc)(o => weightTile(weightAddrHead + o)).reverse
                      val psum:   Array[Int]        = PEArray(ifMap, weight)
                      val ofAddr = ofMapAddr(tc, to, th, tw, sth, stw, od)
                      loopAndAddr.put(Loop(tc, to, th, tw, ti, kr, ks, sth, stw, od), AddrIndex(ifAddr, weightAddrHead, ofAddr, ix, iy, ox, oy))
                      if (printProcession) {
                        println("-" * 100)
                        printf(
                          f"od: $od%-4d stw: $stw%-4d sth: $sth%-4d ks: $ks%-4d kr: $kr%-4d ti: $ti%-4d tw: $tw%-4d th: $th%-4d  to: $to%-4d tc: $tc%-4d\n"
                        )
                        printf(f"ix: $ix%-4d iy: $iy%-4d ifAddr: $ifAddr%-5d weightAddrHead: $weightAddrHead%-5d\n")
                        printf(f"oh: ${th + sth}%-4d ow: ${tw + stw}%-4d ofMapAddr: $ofAddr%-5d\n")
                      }
                      if (tw + stw < Nohw && th + sth < Nohw) {
                        psum.zipWithIndex.foreach { case (p, i) => ofMapTile(ofAddr)(i) += p }
                      }

                    }
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

  def ifMap2Mem(ifMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = { // ic -> ih -> iw -> id
    val ret = Array.fill(NicDUicCeil * ifMapSize)(Array.fill(Uic)(0))
    for (ic <- 0 until Nic; ih <- 0 until Nihw; iw <- 0 until Nihw; id <- 0 until Nid) {
      ret((ic / Uic) * ifMapSize + ih * Nihw * Nid + iw * Nid + id)(ic % Uic) = ifMap(ic)(ih)(iw)(id)
    }
    ret
  }

  def weight2Mem(weight: Array[Array[Array[Array[Int]]]]): Array[Array[Array[Int]]] = { // oc -> ic -> r -> s
    val ret = Array.fill(NocDTcCeil)(Array.fill(NicDUicCeil * TcDUocCeil * kernelSize * Uoc)(Array.fill(Uic)(0)))
    for (oc <- 0 until Nc; ic <- 0 until Nic; kr <- 0 until K; ks <- 0 until K) {
      val tc = oc / Tc
      val to = (oc % Tc) / Uoc
      ret(tc)(to * NicDUicCeil * kernelSize * Uoc + (ic / Uic) * kernelSize * Uoc + kr * Uoc * K + ks * Uoc + (Uc - 1 - (oc % Tc) % Uc))(ic % Uic) =
        weight(oc)(ic)(kr)(ks)
    }
    ret
  }

  def ofMap2Mem(ofMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(NocDUocCeil * ofMapSize)(Array.fill(Uc)(0))
    for (oc <- 0 until Nc; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nod) {
      ret((oc / Uc) * ofMapSize + oh * Nohw * Nod + ow * Nod + od)(oc % Uc) = ofMap(oc)(oh)(ow)(od)
    }
    ret
  }

  def loop(ifMap: Array[Array[Array[Array[Int]]]], weight: Array[Array[Array[Array[Int]]]]): Array[Array[Array[Array[Int]]]] = { // ifMap: ic -> ih -> iw -> id; weight: oc -> ic -> r -> s
    require(ifMap.length  == Nic && ifMap.head.length == Nihw && ifMap.head.head.length == Nihw && ifMap.head.head.head.length == Nid, "ifMap size err")
    require(weight.length == Nc && weight.head.length == Nic && weight.head.head.length == K && weight.head.head.head.length   == K, "weight size err")

    val ofMap = Array.fill(Nc)(Array.fill(Nohw)(Array.fill(Nohw)(Array.fill(Nod)(0)))) // oc -> oh -> ow -> od

    for (oc <- 0 until Nc; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nod) {
      var tmp = 0
      for (kr <- 0 until K; ks <- 0 until K) {
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

  private def weightAddr(to: Int, ti: Int, kr: Int, ks: Int): Int =
    to * NicDUicCeil * Uc * kernelSize + ti * Uc * kernelSize + kr * K * Uc + ks * Uc

  private def ifMapAddr(ti: Int, th: Int, tw: Int, kr: Int, ks: Int, sth: Int, stw: Int, od: Int): Int =
    ti * ifMapSize +
      th * stride * Nihw * Nid +
      tw * stride * Nid +
      kr * Nihw * Nid + ks * Nid +
      sth * stride * Nihw * Nid +
      stw * stride * Nid +
      od - padding * Nihw * Nid - padding * Nid

  private def ofMapAddr(tc: Int, to: Int, th: Int, tw: Int, sth: Int, stw: Int, od: Int): Int = {
    tc * TcDUocCeil * ofMapSize +
      to * ofMapSize +
      th * Nohw * Nod +
      tw * Nod +
      sth * Nohw * Nod +
      stw * Nod +
      od
  }

}
