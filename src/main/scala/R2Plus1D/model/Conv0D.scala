package R2Plus1D.model
import scala.language.postfixOps
import scala.util.Random.nextInt

case class Conv0D(config: ConvConfig) {
  import config._

  def randomIfMap:  Array[Array[Array[Array[Int]]]] = Array.fill(Nic)(Array.fill(Nihw)(Array.fill(Nihw)(Array.fill(Nid)(nextInt(20) - 10))))
  def randomWeight: Array[Array[Int]]               = Array.fill(Noc)(Array.fill(Nic)(nextInt(20) - 10))
  def ifMap2Mem(ifMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val mem = Array.fill(ifMapSize * NicDUicCeil)(Array.fill(Uic)(0))
    for (ic <- 0 until Nic; ih <- 0 until Nihw; iw <- 0 until Nihw; id <- 0 until Nid) {
      mem((ic / Uic) * ifMapSize + ih * Nihw * Nid + iw * Nid + id)(ic % Uic) = ifMap(ic)(ih)(iw)(id)
    }
    mem
  }

  def weight2Mem(weight: Array[Array[Int]]): Array[Array[Int]] = {
    val mem = Array.fill(NicDUicCeil * NocDUocCeil * Uoc)(Array.fill(Uic)(0))
    for (oc <- 0 until Noc; ic <- 0 until Nic) {
      mem((oc / Uoc) * NicDUicCeil * Uoc + (ic / Uic) * Uoc + oc % Uoc)(ic % Uic) = weight(oc)(ic)
    }
    mem
  }

  def ofMap2Mem(ofMap: Array[Array[Array[Array[Int]]]]): Array[Array[Int]] = {
    val mem = Array.fill(NocDUocCeil * ofMapSize)(Array.fill(Uoc)(0))
    for (oc <- 0 until Noc; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nod) {
      mem((oc / Uoc) * ofMapSize + oh * Nohw * Nod + ow * Nod + od)(oc % Uoc) = ofMap(oc)(oh)(ow)(od)
    }
    mem
  }

  def loop(ifMap: Array[Array[Array[Array[Int]]]], weight: Array[Array[Int]]): Array[Array[Array[Array[Int]]]] = { // ic -> ih -> iw -> id, oc ->ic
    val ofMap = Array.fill(Noc)(Array.fill(Nohw)(Array.fill(Nohw)(Array.fill(Nod)(0)))) // oc -> oh -> ow -> od
    for (oc <- 0 until Noc; ic <- 0 until Nic; oh <- 0 until Nohw; ow <- 0 until Nohw; od <- 0 until Nod) {
      val ifMapPixel = ifMap(ic)(oh * 2)(ow * 2)(od * 2)
      val w          = weight(oc)(ic)
      ofMap(oc)(oh)(ow)(od) += ifMapPixel * w

    }
    ofMap
  }

  def loopUnroll(ifMapMem: Array[Array[Int]], weightMem: Array[Array[Int]], printProcession: Boolean = true): Array[Array[Int]] = {
    val ofMapMem = Array.fill(Nohw * Nohw * Nod * NocDUocCeil)(Array.fill(Uoc)(0))
    for (to <- 0 until NocDUocCeil) {
      for (od <- 0 until Nod) {
        for (ti <- 0 until NicDUicCeil) {
          val weightAddrHead = to * NicDUicCeil * Uoc + ti * Uoc
          for (oh <- 0 until Nohw; ow <- 0 until Nohw) {
            val id        = od * 2
            val iw        = ow * 2
            val ih        = oh * 2
            val ifMapAddr = ti * ifMapSize + ih * Nihw * Nid + iw * Nid + id
            val ifMap     = ifMapMem(ifMapAddr)
            val weight    = Array.tabulate(Uoc)(u => weightMem(weightAddrHead + u))
            val sums      = weight.map(w => w.zip(ifMap).map { case (wp, i) => wp * i }.sum)
            val ofMapAddr = to * ofMapSize + oh * Nohw * Nod + ow * Nod + od
            sums.zipWithIndex.foreach { case (s, i) => ofMapMem(ofMapAddr)(i) += s }
            if (printProcession) {
              println("-" * 100)
              printf(f"to: $to%-4d od: $od%-4d ti: $ti%-4d oh: $oh%-4d ow: $ow%-4d\n")
              printf(f"ifMapAddr: $ifMapAddr%-4d weightAddrHead: $weightAddrHead%-4d ofMapAddr: $ofMapAddr%-4d\n")
            }
          }
        }
      }
    }
    ofMapMem
  }

}
