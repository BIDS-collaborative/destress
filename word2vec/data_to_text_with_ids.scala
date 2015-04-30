import java.io._
import utils._

var fnum = 1;

var nfiles = 1131;

var b = new StringBuilder;

while(fnum <= nfiles) {

  println(fnum);

  var dict = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");
  var sents = loadSMat("/var/local/destress/featurized_sent/data" + fnum + "_sent.smat.lz4")
  var dense = loadIMat("/var/local/destress/featurized_sent/data" + fnum + ".imat")

  var (_, nsents) = size(sents);

  val pw = new PrintWriter(new File("/var/local/destress/text_sent_ids/sents_" + fnum + ".txt"));

  var ix = 0;

  while(ix < nsents) {
    var curr = IMat(FMat(sents(find(sents(?, ix) != 0), ix)));
    var z = dict(curr).t;

    b = new StringBuilder;

    // add user id to beginning, middle, and end of sentence
    var user_id = "USER_" + dense(0,ix);
    
    b.append(user_id);
    b.append(" ");

    var j = 0;
    while(j < z.size) {
      b.append(z(j));
      b.append(" ");

      if(j == z.size / 2 - 1) {
        b.append(user_id);
        b.append(" ");
      }

      j += 1;
    }

    b.append(user_id);

    pw.write(b.toString());
    pw.write("\n");

    ix += 1;
  }

  pw.close();

  fnum += 1;
}
