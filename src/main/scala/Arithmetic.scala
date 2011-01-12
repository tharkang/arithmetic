package mk262968

import java.io.DataInputStream
import java.io.FileInputStream
import java.io.DataOutputStream
import java.io.FileOutputStream

object Arithmetic {
  def encode(input: String, output: String) = {
    var best:(Double, MetaData) = (Double.PositiveInfinity, null);
    for(n <- 1 to 20) {
      val dataInput = new DataInputStream(new FileInputStream(input));
      val k0 = 20/n - 1;
      println("Calculating metadata for n=%d".format(n));
      var meta = MetaData.fromIterable(n, k0, Packer.load(dataInput, n));
      for(kd <- 0 to k0) {
        //println("Estimated csize = %f\t(meta %d, bits %d)".format(meta.expectedSize, meta.metaSize, meta.bitsCount));
        val k = k0 - kd;
        val os = meta.overallSize;
        if(os < best._1)
          best = (os, meta);
        if (k > 0) {
          //println("Obtaining metadata for n=%d k=%d".format(n, k - 1));
          meta = meta.decreasedk;
        }
      }
    }
    println("Best available estimated compression: %fB (n = %d, k = %d)".format(best._1/8, best._2.n, best._2.k));
    println("Compressing");
    val dataInput = new DataInputStream(new FileInputStream(input));
    val dataOutput = new DataOutputStream(new FileOutputStream(output));
    println("%d bytes".format(dataInput.available));
    dataOutput.writeInt(dataInput.available);
    println("Saving metadata");
    best._2.save(dataOutput);
    val n = best._2.n;
    val encoder = new Encoder(Packer.load(dataInput, n), best._2);
    println("Saving data");
    Packer.save(dataOutput, encoder, 1);
    println("Finished");
    dataOutput.flush();
    dataOutput.close();
  }
  def main(args: Array[String]) = {
    if(args.length < 1)
      println("Bad usage");
    if(args(0) == "encode") {
      val inputPath = args(1);
      val outputPath = if(args.length > 2) args(2) else inputPath + ".xD";
      encode(inputPath, outputPath);
    }
    else {
      println("Unknown action %s".format(args(0)));
    }
  }
}


// vim: set ts=2 sw=2 et:
