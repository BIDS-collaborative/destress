import utils._
import memQuery._

var percData = 0.01;  // no more than 0.012
var corpus = 0;  // select 1-for LJ; 0-for Google
var seed = 94720;  // default is 94720
// Loads, converts to w2v and normalizes
var (dict, dataMat, sents, w2vMat, nValidSents, ids) = loadMemSentences_CPU(percData, corpus, seed)
// Sizes
// dataMat -> FMat 300 x #sentences
// sents -> SMat 500 x #sentences
// w2vMat -> FMat 300 x #words
// nValidSents -> Int
// ids -> IMat 3 x #sentences


def query( query_s : String , top : Int, filter: String = null) = {

  var query_vec = FMat(size(w2vMat, 1), 1);

  // Converts input query to dictionary indexes
  var ss = query_s.toLowerCase().split(" ")

  // Convert input query to a word2vec vector
  for(s <- ss) {
    if(dict(s) == -1) {
      printf("WARNING: did not find %s in master dict\n", s);
    } else {
      var vec = w2vMat(?, dict(s));

      if(sum(vec^2)(0) == 0) {
        printf("WARNING: %s is not in word2vec database\n", s);
      } else {
        printf("adding %s to vector\n", s);
        query_vec += vec;
      }
    }
  }
  // Normalize
  // size 300x1
  query_vec = query_vec / norm(query_vec);
  println();

  // Compute the score of each sentence
  // Note that due to normalization, dataMat has NaNs
  // Need to filter res==NaN
  var res = query_vec.t * dataMat;  // 1x#sentences
  res(find((1-(res<=0))*@(1-(res>0)))) = -1; // sentence sums to 0

  // Sort Results to Return Top Ones
  var (x, bestIndex) = sortdown2(res);

  var nwords = size(sents)(0);
  var prev_res = -1f;

  var i = 0;
  var count = 0;
  // for(i <- 0 until bestIndex.length) {
  while(count < top) {
    var ix = bestIndex(i);
    var curr = IMat(FMat(sents(find(sents(?, ix)), ix)));
    var z = dict(curr).t;
    var sent = (z ** csrow(" ")).toString().replace(" ,", " ");

    if(res(ix) != prev_res) { // discard repeated strings?
      prev_res = res(ix);
      if (filter == null || !sent.contains(filter)) {
        printf("%.3f -- %s\n", res(ix), sent);
        count += 1;
      }
    }
    // else {
    //   printf("ignoring %s\n", sent);
    // }
    i += 1;
  }
  println();
}

// Example usage:
// query("cancer", 20)
// query("amazing", 10)
