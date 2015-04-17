import utils._


var dict = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");
var data = loadSMat("/var/local/destress/featurized_sent/data1.smat.lz4");
var sents = loadSMat("/var/local/destress/featurized_sent/data1_sent.smat.lz4")
var googleVecs = loadFMat("/var/local/destress/wordvec_google_2.fmat")

var magic = data.t * googleVecs;
var n = sum(magic^2, 2);
var nmagic = magic / sqrt(n);

def query( query_s : String , top : Int) : Int = {

  var query_vec = googleVecs(0, ?) * 0;

  var ss = query_s.split(" ");
  var s = "";
  for(s <- ss) {
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

  res(find(res > 0.9999)) = -1; // single word, not interesting

  var (x, ind) = sortdown2(res);
  var bestIndex = ind(0 until top);

  for(i <- 0 until bestIndex.length) {
    var ix = bestIndex(i);
    var z = dict(IMat(FMat(sents(find(sents(?, ix) != 0), ix)))).t;
    var sent = (z ** csrow(" ")).toString().replace(" ,", " ");
    printf("%.3f -- %s\n", res(ix), sent);
  }
  println();

  return bestIndex.length;
}

// Example usage:
// query("cancer", 20)
// query("amazing", 10)
