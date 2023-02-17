package R2Plus1D.model

import scala.language.postfixOps
import scala.math.ceil
import util.Random.nextInt

case class Conv1D(config: ConvConfig) {
  import config._
  val Nc        = config.Nic
  val NcDUcCeil = config.NicDUicCeil
  val Uc        = config.Uic
  def randomIfMap:  Array[Array[Array[Array[Int]]]] = Array.fill(Nc)(Array.fill(Nid)(Array.fill(Nihw)(Array.fill(Nihw)(nextInt(10))))) // ic -> id -> ih -> iw
  def randomWeight: Array[Array[Array[Int]]]        = Array.fill(Noc)(Array.fill(Nc)(Array.fill(K)(nextInt(10)))) // oc -> ic -> K

  def ifMap2Tile(ifMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(NcDUcCeil * ifMapSize)(Array.fill(Uc)(0))
    for (ic <- 0 until Nc; id <- 0 until Nid; ih <- 0 until Nihw; iw <- 0 until Nihw) {
      ret(ic / Uc * ifMapSize + ih * Nid * Nihw + iw * Nid + id)(ic % Uc) = ifMap(ic)(id)(ih)(iw)
    }
    ret
  }

  def weight2Tile(weight: Array[Array[Array[Int]]]): Array[Array[Int]] = {
    val ret = Array.fill(NocDUocCeil * NcDUcCeil * K * Uoc)(Array.fill(Uc)(0))
    for (oc <- 0 until Noc; ic <- 0 until Nc; t <- 0 until K) {
      ret((oc / Uoc) * divideCeil(Nc, Uc) * K * Uoc + (ic / Uc) * K * Uoc + t * Uoc + oc % Uoc)(ic % Uc) = weight(oc)(ic)(t)
    }
    ret
  }

  def loopUnroll(ifMapTile: Array[Array[Int]], weightTile: Array[Array[Int]], printProcession: Boolean = true): Array[Array[Int]] = {

    val ofMapTile = Array.fill(divideCeil(Noc, Uoc) * ofMapSize)(Array.fill(Uoc)(0))

    for (to <- 0 until NocDUocCeil) {
      for (od <- 0 until Nod) {
        for (ti <- 0 until NcDUcCeil) {
          for (k <- 0 until K) {
            val weightAddrHead = to * divideCeil(Nc, Uc) * Uoc * K + ti * Uoc * K + k * Uoc
            for (oh <- 0 until Nohw; ow <- 0 until Nohw) {
              val id        = od * stride + k - padding
              val ifMapAddr = ti * ifMapSize + oh * Nihw * Nid + ow * Nid + id
              val ifMap     = if (id >= Nid || id < 0) Array.fill(Uc)(0) else ifMapTile(ifMapAddr)
              val weight    = Array.tabulate(Uoc)(o => weightTile(weightAddrHead + o))
              val psum      = PEArray(ifMap, weight)
              val ofMapAddr = to * ofMapSize + oh * Nohw * Nod + ow * Nod + od
              psum.zipWithIndex.foreach { case (p, i) => ofMapTile(ofMapAddr)(i) += p }
              if (printProcession) {
                println("-" * 100)
                printf(f"ow: $ow%-4d oh: $oh%-4d k: $k%-4d  ti: $ti%-4d  od: $od%-4d to: $to%-4d\n")
                printf(f"id: $id%-4d ifAddr: $ifMapAddr%-4d weightAddrHead: $weightAddrHead%-4d ofMapAddr: $ofMapAddr%-4d\n")
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
    for (oc <- 0 until Noc; ic <- 0 until Nc) {
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
    val ret = Array.fill(divideCeil(Noc, Uoc) * ofMapSize)(Array.fill(Uoc)(0))
    for (oc <- 0 until Noc; od <- 0 until Nod; oh <- 0 until Nohw; ow <- 0 until Nohw) {
      ret(oc / Uoc * ofMapSize + oh * Nohw * Nod + ow * Nod + od)(oc % Uoc) = ofMap(oc)(od)(oh)(ow)
    }
    ret
  }

  private def divideCeil(a: Int, b: Int): Int = ceil(a.toDouble / b.toDouble).toInt

}
