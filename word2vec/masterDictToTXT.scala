import utils._
import scalax.io._
import java.io._

val output:Output = Resource.fromFile("new.txt")

val v = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");


for (i <- 0 to 994948) {
   output.write(v(i)+ "\n");
}

