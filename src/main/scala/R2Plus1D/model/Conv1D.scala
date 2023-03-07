package R2Plus1D.model

import scala.language.postfixOps
import scala.math.ceil
import util.Random.nextInt

case class Conv1D(config: ConvConfig) {
  import config._
  def randomIfMap:  Array[Array[Array[Array[Int]]]] = Array.fill(Nic)(Array.fill(Nid)(Array.fill(Nihw)(Array.fill(Nihw)(nextInt(10))))) // ic -> id -> ih -> iw
  def randomWeight: Array[Array[Array[Int]]]        = Array.fill(Noc)(Array.fill(Nic)(Array.fill(K)(nextInt(10)))) // oc -> ic -> K

  def ifMap2Mem(ifMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(NicDUicCeil * ifMapSize)(Array.fill(Uic)(0))
    for (ic <- 0 until Nic; id <- 0 until Nid; ih <- 0 until Nihw; iw <- 0 until Nihw) {
      val addr: Int = ic / Tc * Uic * ifMapSize + (ic % Tc) / Uic * ifMapSize + ih * Nid * Nihw + iw * Nid + id //
      if (addr < ret.length)
        ret(addr)(ic % Uic) = ifMap(ic)(id)(ih)(iw)
    }
    ret
  }

  def weight2Mem(weight: Array[Array[Array[Int]]]): Array[Array[Array[Int]]] = {
    val ret: Array[Array[Array[Int]]] = Array.fill(NicDTcCeil)(Array.fill(TcDUicCeil * NocDUocCeil * Uoc * kernelSize)(Array.fill(Uic)(0)))
    for (oc <- 0 until Noc; ic <- 0 until Nic; t <- 0 until K) {
      val tc = ic / Tc
      val ti = (ic % Tc) / Uic
      val to = oc / Uoc
      ret(tc)(to * TcDUicCeil * K * Uoc + ti * K * Uoc + t * Uoc + oc % Uoc)(ic % Uic) = weight(oc)(ic)(t)
    }
    ret
  }

  def loopUnroll(ifMapMem: Array[Array[Int]], weightMem: Array[Array[Array[Int]]], printProcession: Boolean = true): Array[Array[Int]] = {

    val ofMapTile = Array.fill(NocDUocCeil * ofMapSize)(Array.fill(Uoc)(0))

    for (to <- 0 until TcDUicCeil) {
      for (od <- 0 until Nod) {
        for (tc <- 0 until NicDTcCeil) {
          val weightTile = weightMem(tc)
          for (ti <- 0 until TcDUicCeil) {
            if (tc * Tc + ti * Uic < NicDUicCeil * Uic) {
              for (k <- 0 until K) {
                val weightAddrHead = to * TcDUicCeil * Uoc * K + ti * Uoc * K + k * Uoc
                for (oh <- 0 until Nohw; ow <- 0 until Nohw) {
                  val id        = od * stride + k - padding
                  val ifMapAddr = tc * TcDUicCeil * ifMapSize + ti * ifMapSize + oh * Nihw * Nid + ow * Nid + id
                  println(ifMapAddr)
                  val ifMap     = if (id >= Nid || id < 0) Array.fill(Uic)(0) else ifMapMem(ifMapAddr)
                  val weight    = Array.tabulate(Uoc)(o => weightTile(weightAddrHead + o))
                  val psum      = PEArray(ifMap, weight)
                  val ofMapAddr = to * ofMapSize + oh * Nohw * Nod + ow * Nod + od
                  psum.zipWithIndex.foreach { case (p, i) => ofMapTile(ofMapAddr)(i) += p }
                  if (printProcession) {
                    println("-" * 100)
                    printf(f"ow: $ow%-4d oh: $oh%-4d k: $k%-4d  ti: $ti%-4d tc: $tc%-4d od: $od%-4d to: $to%-4d\n")
                    printf(f"id: $id%-4d ifAddr: $ifMapAddr%-4d weightAddrHead: $weightAddrHead%-4d ofMapAddr: $ofMapAddr%-4d\n")
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

  def loop(ifMap: Array[Array[Array[Array[Int]]]], weight: Array[Array[Array[Int]]]): Array[Array[Array[Array[Int]]]] = {
    val ofMap: Array[Array[Array[Array[Int]]]] = Array.fill(Noc)(Array.fill(Nod)(Array.fill(Nohw)(Array.fill(Nohw)(0)))) // oc -> od -> oh -> ow
    for (oc <- 0 until Noc; ic <- 0 until Nic) {
      for (od <- 0 until Nod; K <- 0 until K) {
        val id = od * stride + K - padding
        for (oh <- 0 until Nohw; ow <- 0 until Nohw) {
          val ifMapPixel: Int = if (id < 0 || id >= Nid) 0 else ifMap(ic)(id)(oh)(ow)
          ofMap(oc)(od)(oh)(ow) += ifMapPixel * weight(oc)(ic)(K)
        }
      }
    }
    ofMap
  }

  def ofMap2Tile(ofMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(NocDUocCeil * ofMapSize)(Array.fill(Uoc)(0))
    for (oc <- 0 until Noc; od <- 0 until Nod; oh <- 0 until Nohw; ow <- 0 until Nohw) {
      ret(oc / Uoc * ofMapSize + oh * Nohw * Nod + ow * Nod + od)(oc % Uoc) = ofMap(oc)(od)(oh)(ow)
    }
    ret
  }

}
