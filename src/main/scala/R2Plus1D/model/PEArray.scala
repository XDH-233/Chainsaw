package R2Plus1D.model

object PEArray {
  def apply(ifMap: Array[Int], weight: Array[Array[Int]]): Array[Int] = weight.map(W => W.zip(ifMap).map { case (w, i) => w * i }.sum)
}
