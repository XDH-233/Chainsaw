package R2Plus1D.model

import scala.math.ceil
import util.Random.nextInt
case class ConvInsideTile1D(config: ConvInsideTile1DConfig) { // TODO
  import config._

  private val ifMapSize: Int = Nid * Nhw * Nhw
  private val ofMapSize: Int = Nod * Nhw * Nhw

  def randomIfMap:  Array[Array[Array[Array[Int]]]] = Array.fill(Tc)(Array.fill(Nid)(Array.fill(Nhw)(Array.fill(Nhw)(nextInt(10))))) // ic -> id -> ih -> iw
  def randomWeight: Array[Array[Array[Int]]]        = Array.fill(Noc)(Array.fill(Tc)(Array.fill(Kt)(nextInt(10)))) // oc -> ic -> kt

  def ifMap2Tile(ifMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Tc, Uc) * ifMapSize)(Array.fill(Uc)(0))
    for (ic <- 0 until Tc; id <- 0 until Nid; ih <- 0 until Nhw; iw <- 0 until Nhw) {
      ret(ic / Uc * ifMapSize + ih * Nid * Nhw + iw * Nid + id)(ic % Uc) = ifMap(ic)(id)(ih)(iw)
    }
    ret
  }

  def weight2Tile(weight: Array[Array[Array[Int]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Noc, Uoc) * divideCeil(Tc, Uc) * Kt * Uoc)(Array.fill(Uc)(0))
    for (oc <- 0 until Noc; ic <- 0 until Tc; t <- 0 until Kt) {
      ret((oc / Uoc) * divideCeil(Tc, Uc) * Kt * Uoc + (ic / Uc) * Kt * Uoc + t * Uoc + oc % Uoc)(ic % Uc) = weight(oc)(ic)(t)
    }
    ret
  }

  def loopUnroll(ifMapTile: Array[Array[Int]], weightTile: Array[Array[Int]]): Array[Array[Int]] = {

//    println("----------input tile-------------------")
//    ifMapTile.foreach(i => println(i.mkString(" ")))
//    println("------------weight tile ------------------")
//    weightTile.foreach(w => println(w.mkString(" ")))

    val ofMapTile = Array.fill(divideCeil(Noc, Uic) * ofMapSize)(Array.fill(Uic)(0))

    for (to <- 0 until divideCeil(Noc, Uoc); ti <- 0 until divideCeil(Tc, Uc)) {
      for (od <- 0 until Nod; kt <- 0 until Kt) {
        val weightAddrHead = to * divideCeil(Tc, Uc) * Uoc * Kt + ti * Uoc * Kt + kt * Uoc
        for (oh <- 0 until Nhw; ow <- 0 until Nhw) {
          val id         = od * stride + kt - padding
          val ifMapAddr  = ti * ifMapSize + oh * Nhw * Nid + ow * Nid + id
          val ifMap      = if (id >= Nid || id < 0) Array.fill(Uc)(0) else ifMapTile(ifMapAddr)
          val weight     = Array.tabulate(Uoc)(o => weightTile(weightAddrHead + o))
          val psum       = PEArray(ifMap, weight)
          val ofMapAddr0 = to * divideCeil(Uoc, Uc) * ofMapSize + oh * Nhw * Nod + ow * Nod + od
//          println(s"to:$to, ti:$ti, od:$od, kt:$kt, oh$oh, ow$ow")
//          println(s"id:$id, ifAddr:$ifMapAddr, weightAddr:$weightAddrHead")
//          println("ifMap: " + ifMap.mkString(" "))
//          println("---------weight -------------")
//          weight.foreach(w => println(w.mkString(" ")))
//          println(" ")
          psum.grouped(Uic).zipWithIndex.foreach { case (g, i) => g.zipWithIndex.foreach { case (p, j) => ofMapTile(ofMapAddr0 + i * ofMapSize)(j) += p } }
        }
      }
    }
    ofMapTile
  }

  def loop(ifMap: Array[Array[Array[Array[Int]]]], weight: Array[Array[Array[Int]]]): Array[Array[Array[Array[Int]]]] = {
    val ofMap: Array[Array[Array[Array[Int]]]] = Array.fill(Noc)(Array.fill(Nod)(Array.fill(Nhw)(Array.fill(Nhw)(0)))) // oc -> od -> oh -> ow
    for (oc <- 0 until Noc; ic <- 0 until Tc) {
      for (od <- 0 until Nod; kt <- 0 until Kt) {
        val id = od * stride + kt - padding
        for (oh <- 0 until Nhw; ow <- 0 until Nhw) {
          val ifMapPixel: Int = if (id < 0 || id >= Nid) 0 else ifMap(ic)(id)(oh)(ow)
          ofMap(oc)(od)(oh)(ow) += ifMapPixel * weight(oc)(ic)(kt)
//          println(s"oc: $oc, ic:$ic, od:$od, kt:$kt, oh: $oh, ow: $ow")
//          println(s"if:$ifMapPixel, weight:${weight(oc)(ic)(kt)}")
//          println(" ")
        }
      }
    }
    ofMap
  }

  def ofMap2Tile(ofMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val ret = Array.fill(divideCeil(Noc, Uic) * ofMapSize)(Array.fill(Uic)(0))
    for (oc <- 0 until Noc; od <- 0 until Nod; oh <- 0 until Nhw; ow <- 0 until Nhw) {
      ret(oc / Uic * ofMapSize + oh * Nhw * Nod + ow * Nod + od)(oc % Uic) = ofMap(oc)(od)(oh)(ow)
    }
    ret
  }

  private def divideCeil(a: Int, b: Int): Int = ceil(a.toDouble / b.toDouble).toInt

}
