package mk262968
import scala.annotation.tailrec

object RangeHelper {
  implicit def rangeHelper(lrd: (BigInt, BigInt, BigInt)) = new {
    def goIn(nl: Int, nr: Int, nd: Int): (BigInt, BigInt, BigInt) = {
      val (left, right, denom) = lrd;
      //val cl = denom * nl + left * (nr - nl);
      //val cr = denom * nl + right * (nr - nl);
      //val cd = denom * nd;
      val cl = left * nd - denom * nl;
      val cr = right * nd - denom * nl;
      val cd = denom*(nr - nl);
      val g = (cl gcd cr) gcd cd;
      (cl/g, cr/g, cd/g)
    }
  }
}

class Encoder(packs: Iterable[Int], val meta: MetaData) extends Iterable[Int]{
  import RangeHelper._
  override def iterator = new Iterator[Int] {
    var left = BigInt(0);
    var right = BigInt(1);
    var denom = BigInt(1);
    val it = packs.iterator;
    var helper = meta.helper;
    var subrange: Option[(Int, Int, Int)] = nextRange
    var iii = 0;
    def info: Unit = {
      iii += 1;
      if(iii%1000 == 0)
        Console.println("On %d byte x %d".format(iii, denom.bitLength));
    }

    def nextRange = 
      if(it.hasNext) {
        val i = it.next;
        info;
        val a = helper.get;
        val z = a.take(i).sum;
        val r = Some(z, z + a(i), a.sum);
        helper = helper.choose(i);
        r
      }
      else
        None;

    override def hasNext = subrange match {
      case Some(_) => true
      case None => left < 0
    }

    def selectHalf: BigInt = {
      val h = left + right;
      if(!h.testBit(0))
        (h >> 1)
      else {
        left <<= 1;
        right <<= 1;
        denom <<= 1;
        h 
      }
    }

    override def next: Int = {
      //println(subrange);
      //println(denom - right);
      @tailrec
      def f: Int = {
        if(right < left)
          throw new Exception("left right inversion %s %s %s".format(left.toString, right.toString, denom.toString));
        subrange match {
          case Some((rl, rr, rd)) => {
            val c = selectHalf;

            if(c*rd <= denom*rl) { // c/denom <= rl/rd
              left = c;
              1
            }
            else if(c*rd > denom*rr) { // c/denom > rr/rd
              //println("RIGHT ACTION!");
              right = c;
              0
            }
            else {
              val (nl, nr, nd) = (left, right, denom) goIn (rl, rr, rd);
              left = nl;
              right = nr;
              denom = nd;
              if(it.hasNext) {
                subrange = nextRange;
              }
              else {
                subrange = None;
              }
              f
            }
          }
        
          case None => {
            // we don't have to get any deeper, just move left side into range
            val c = selectHalf;
            left = c;
            1
          }
        }
      }
      f
    }
  }
}

/* Decodes bit stream, representing number from [0, 1] range into infinite sequence of symbols */
class Decoder(bits: Iterable[Int], val meta: MetaData) extends Iterable[Int] {
  import RangeHelper._
  override def iterator = new Iterator[Int] {
    var it = bits.iterator;
    var left = BigInt(0);
    var right = BigInt(1);
    var denom = BigInt(1);

    var helper = meta.helper;

    var rtab = helper.get;
    var rsum = rtab.sum;  // denominator of subrange
    var ri = 0;   // index of subrange where working range left side falls into
    var rv = 0;   // nominator of subrange left side

    def selectHalf = {
      val h = left + right;
      if(!h.testBit(0))
        h >> 1
      else {
        left <<= 1;
        right <<= 1;
        denom <<= 1;
        h 
      }
    }

    override def hasNext = true

    override def next = {
      @tailrec
      def f: Int = {
        while( denom*(rv + rtab(ri)) <= left*rsum ) { // not this range
          rv += rtab(ri);
          ri += 1;
        }
        if(right*rsum < denom*(rv + rtab(ri))) { // our range fits into the subrange
          val (nl, nr, nd) = (left, right, denom) goIn (rv, rv + rtab(ri), rsum);
          left = nl;
          right = nr;
          denom = nd;
          val r = ri;
          helper = helper.choose(r);
          rtab = helper.get;
          rsum = rtab.sum;
          ri = 0;
          rv = 0;
          r
        }
        else {
          val c = selectHalf;
          if(!it.hasNext || it.next == 0)
            right = c;
          else
            left = c;
          f
        }
      }
      f
    }
  }
}

// vim: set ts=2 sw=2 et:
