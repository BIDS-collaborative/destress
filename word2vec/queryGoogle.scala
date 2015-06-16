import utils._
import java.io._

var dict = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");
//var dict = loadDict("/var/local/destress/text_tokenized/masterDict.sbmat");
var data = loadSMat("/var/local/destress/featurized_sent/data1.smat.lz4");
var sents = loadSMat("/var/local/destress/featurized_sent/data1_sent.smat.lz4")
var googleVecs = loadFMat("/var/local/destress/google_training/wordvec_google_2.fmat")

var magic = data.t * googleVecs;
var n = sum(magic^2, 2);
var nmagic = magic / sqrt(n);

def query( query_s : String , top : Int) = {

  var file = "illnessGoogle.txt"
  val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)));

  var query_vec = googleVecs(0, ?) * 0;

  var ss = query_s.split(" ");
  var s = "";

  for(x <- ss) {
    s = x.toLowerCase();

    if(dict(s) == -1) {
      printf("WARNING: did not find %s in master dict\n", s);
    } else {
      var vec = googleVecs(dict(s), ?);

      if(sum(vec^2)(0) == 0) {
        printf("WARNING: %s is not in google wordvec database\n", s);
      } else {
        printf("adding %s to vector\n", s);
        query_vec += vec;
      }
    }
  }

  println();

  query_vec = query_vec / sqrt(sum(query_vec^2));

  var res = nmagic * query_vec.t;

  res(find(n == 0)) = -1; // sentence sums to 0

  // res(find(res > 0.9999)) = -1; // single word, not interesting

  var (x, bestIndex) = sortdown2(res);
  // var bestIndex = ind(0 until top);

  var nwords = size(sents)(0);
  var prev = "   ";
  var prev_res = -1f;

  var i = 0;
  var count = 0;
  var res_list = List();
  // for(i <- 0 until bestIndex.length) {
  while(count < top) {
    var ix = bestIndex(i);
    var curr = IMat(FMat(sents(find(sents(?, ix) != 0), ix)));
    var z = dict(curr).t;
    var sent = (z ** csrow(" ")).toString().replace(" ,", " ");

    // if(sent.substring(0, sent.length-2) != prev.substring(0, prev.length-2)) {
    if(res(ix) != prev_res) {
      prev = sent;
      prev_res = res(ix);
      printf("%.3f -- %s\n", res(ix), sent);
      var res_str = "%.3f -- %s\n".format(res(ix), sent);
      res_list :+ res_str;
      count += 1;
    }
    // else {
    //   printf("ignoring %s\n", sent);
    // }
    i += 1;
  }
  println();
  for (x <- res_list) {
    writer.write(x);
  }
  writer.close();
}


// Example usage:
// query("cancer", 20)
// query("amazing", 10)
