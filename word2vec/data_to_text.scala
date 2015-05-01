import java.io._
import utils._

var fnum = 1;

var nfiles = 1131;

while(fnum <= nfiles) {

  println(fnum);

  var dict = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");
  var sents = loadSMat("/var/local/destress/featurized_sent/data" + fnum + "_sent.smat.lz4")

  var (_, nsents) = size(sents);

  val pw = new PrintWriter(new File("/var/local/destress/text_sent/sents_" + fnum + ".txt"));

  var ix = 0;

  while(ix < nsents) {
    var curr = IMat(FMat(sents(find(sents(?, ix) != 0), ix)));
    var z = dict(curr).t;
    var sent = (z ** csrow(" ")).toString().replace(" ,", " ");

    pw.write(sent);
    pw.write("\n");

    ix += 1;
  }

  pw.close();

  fnum += 1;
}
