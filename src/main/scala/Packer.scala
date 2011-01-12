package mk262968

import java.io.{DataInputStream, DataOutputStream}

/** Reads file by packs containing n beates each */
class PackConverter(n: Int, iterable: Iterable[Int], m: Int) extends Iterable[Int] {
  val mask = (1L << n) - 1;
  override def iterator = new Iterator[Int]{
    var it = iterable.iterator;
    var buf: Long = 0;
    var bufl: Int = 0;
    def hasNext = bufl > 0 || it.hasNext;
    def next = {
      while(bufl < n && it.hasNext) {
        val z = it.next;
        //Console.println("byte read %d".format(z.toInt));
        buf = (buf << m) | z;
        bufl += m;
      }
      if(bufl >= n) {
        val r = (buf >> (bufl - n)) & mask;
        bufl -= n;
        r.toInt
      }
      else {
        val r = (buf << (n - bufl)) & mask;
        bufl = 0;
        buf = 0;
        r.toInt
      }
    }
  }
}

object Packer {
  class DataInputStreamIterable(input: DataInputStream) extends Iterable[Int] {
    override def iterator = new Iterator[Int] {
      val bufm = 1024;
      var bufs = 0;
      var bufp = 0;
      val buf = Array.fill[Byte](bufm)(0);
      def hasNext = bufp < bufs || input.available > 0
      def next: Int = {
        if(bufp < bufs) {
          bufp += 1;
        }
        else {
          bufp = 1;
          bufs = input.read(buf, 0, bufm);
        };
        buf(bufp - 1).toInt & 0xff
      }
    }
  }
  def load(input: DataInputStream, n: Int): Iterable[Int] = {
    val dsiv = (new DataInputStreamIterable(input)).view;
    if(n == 8)
      dsiv 
    else
      new PackConverter(n, dsiv, 8);
  }
      
  def save(output: DataOutputStream, packs: Iterable[Int], n: Int) = {
    val cp = if(n == 8) packs else new PackConverter(8, packs, n);
    cp.foreach(output.writeByte);
  }
}



// vim: set ts=2 sw=2 et:
