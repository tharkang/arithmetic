package mk262968

import java.io.{DataInputStream, DataOutputStream}

class MetaData(val n: Int, val k: Int, data: Array[Array[Int]]) {
  val mask = ((1.toLong << (n*k)) - 1).toInt;
  def metaSize = 32*(1 << (n*(k+1)))
  def expectedSize: Double = data.map { arr =>
    val s = arr.sum;
    arr.map(v => if(v != 0) v * math.log (s / v.toDouble) else 0.0).sum / math.log(2.0);
  }.sum;
  def overallSize = metaSize + expectedSize
  def save(output: DataOutputStream) = {
    output.writeInt(n);
    output.writeInt(k);
    data foreach {arr => arr foreach {v => output.writeInt(v);}};
  }
  def bitsCount = data.map { arr => arr sum } sum;
  def decreasedk = {
    if(k == 0)
      throw new Exception("k = 0 can't be decreased");
    val ndata = Array.tabulate[Array[Int]](1 << (n*(k-1))){p:Int => Array.tabulate[Int](1 << n){i:Int =>
      (0 until (1 << n)).map{x => data((x << (k-1)*n) | p)(i)}.sum}};
    new MetaData(n, k - 1, ndata)
  }
  class Helper(buf: Int) {
    def get: Array[Int] = data(buf)
    def choose(pack:Int): Helper = new Helper(((buf << n)|pack)&mask)
  }
  def helper = new Helper(0)
}

object MetaData {
  def fromIterable(n: Int, k: Int, iterable: Iterable[Int]) = {
    val mask = ((1.toLong << (n*k)) - 1).toInt;
    var data = Array.fill[Array[Int]](1 << (n*k))(Array.fill[Int](1 << n)(0));
    var buf = 0;
    var i = 0;
    iterable foreach { pack =>
      //val (pack, i) = packi;
      //println("gotit %d %d".format(i, pack));
      data(buf)(pack) += 1;
      buf = ((buf << n) | pack)&mask ;
      i += 1;
    }
    new MetaData(n, k, data)
  }
  def load(input: DataInputStream) = {
    val n = input.readInt();
    val k = input.readInt();
    val data = Array.fill[Array[Int]](1 << (n*k))(Array.fill[Int](1 << n)(input.readInt()));
    new MetaData(n, k, data)
  }
}


// vim: set ts=2 sw=2 et:
