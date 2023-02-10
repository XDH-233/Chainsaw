package R2Plus1D.model

import scala.language.postfixOps
import scala.math.ceil
import util.Random.nextInt
case class Conv1D(config: Conv1DConfig) {
  import config._

  def randomIfMap:  Array[Array[Array[Array[Int]]]] = Array.fill(Nc)(Array.fill(Nid)(Array.fill(Nhw)(Array.fill(Nhw)(nextInt(10))))) // ic -> id -> ih -> iw
  def randomWeight: Array[Array[Array[Int]]]        = Array.fill(Noc)(Array.fill(Nc)(Array.fill(Kt)(nextInt(10)))) // oc -> ic -> kt

  def ifMap2Tile(ifMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Nc, Uc) * ifMapSize)(Array.fill(Uc)(0))
    for (ic <- 0 until Nc; id <- 0 until Nid; ih <- 0 until Nhw; iw <- 0 until Nhw) {
      ret(ic / Uc * ifMapSize + ih * Nid * Nhw + iw * Nid + id)(ic % Uc) = ifMap(ic)(id)(ih)(iw)
    }
    ret
  }

  def weight2Tile(weight: Array[Array[Array[Int]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Noc, Uoc) * divideCeil(Nc, Uc) * Kt * Uoc)(Array.fill(Uc)(0))
    for (oc <- 0 until Noc; ic <- 0 until Nc; t <- 0 until Kt) {
      ret((oc / Uoc) * divideCeil(Nc, Uc) * Kt * Uoc + (ic / Uc) * Kt * Uoc + t * Uoc + (Uoc - 1 - oc % Uoc))(ic % Uc) = weight(oc)(ic)(t)
    }
    ret
  }

  def loopUnroll(ifMapTile: Array[Array[Int]], weightTile: Array[Array[Int]]): Array[Array[Int]] = {

    val ofMapTile = Array.fill(divideCeil(Noc, Uoc) * ofMapSize)(Array.fill(Uoc)(0))

    for (to <- 0 until divideCeil(Noc, Uoc); ti <- 0 until divideCeil(Nc, Uc)) {
      for (od <- 0 until Nod; kt <- 0 until Kt) {
        val weightAddrHead = to * divideCeil(Nc, Uc) * Uoc * Kt + ti * Uoc * Kt + kt * Uoc
        for (oh <- 0 until Nhw; ow <- 0 until Nhw) {
          val id        = od * stride + kt - padding
          val ifMapAddr = ti * ifMapSize + oh * Nhw * Nid + ow * Nid + id
          val ifMap     = if (id >= Nid || id < 0) Array.fill(Uc)(0) else ifMapTile(ifMapAddr)
          val weight    = Array.tabulate(Uoc)(o => weightTile(weightAddrHead + o)).reverse
          val psum      = PEArray(ifMap, weight)
          val ofMapAddr = to * ofMapSize + oh * Nhw * Nod + ow * Nod + od
          psum.zipWithIndex.foreach { case (p, i) => ofMapTile(ofMapAddr)(i) += p }
          println("-" * 100)
          printf(f"ow: $ow%-4d oh: $oh%-4d kt: $kt%-4d od: $od%-4d ti: $ti%-4d\n")
          printf(f"id: $id%-4d ifAddr: $ifMapAddr%-4d weightAddrHead: $weightAddrHead%-4d\n")
        }
      }
    }
    ofMapTile
  }

  def loop(ifMap: Array[Array[Array[Array[Int]]]], weight: Array[Array[Array[Int]]]): Array[Array[Array[Array[Int]]]] = {
    val ofMap: Array[Array[Array[Array[Int]]]] = Array.fill(Noc)(Array.fill(Nod)(Array.fill(Nhw)(Array.fill(Nhw)(0)))) // oc -> od -> oh -> ow
    for (oc <- 0 until Noc; ic <- 0 until Nc) {
      for (od <- 0 until Nod; kt <- 0 until Kt) {
        val id = od * stride + kt - padding
        for (oh <- 0 until Nhw; ow <- 0 until Nhw) {
          val ifMapPixel: Int = if (id < 0 || id >= Nid) 0 else ifMap(ic)(id)(oh)(ow)
          ofMap(oc)(od)(oh)(ow) += ifMapPixel * weight(oc)(ic)(kt)
        }
      }
    }
    ofMap
  }

  def ofMap2Tile(ofMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Noc, Uoc) * ofMapSize)(Array.fill(Uoc)(0))
    for (oc <- 0 until Noc; od <- 0 until Nod; oh <- 0 until Nhw; ow <- 0 until Nhw) {
      ret(oc / Uoc * ofMapSize + oh * Nhw * Nod + ow * Nod + od)(oc % Uoc) = ofMap(oc)(od)(oh)(ow)
    }
    ret
  }

  private def divideCeil(a: Int, b: Int): Int = ceil(a.toDouble / b.toDouble).toInt

}
