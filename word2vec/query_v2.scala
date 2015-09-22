import utils._
import memQuery._

var percData = 0.01;  // no more than 0.012
var corpus = 0;  // select 1-for LJ; 0-for Google
var seed = 94720;  // default is 94720
// Loads, converts to w2v and normalizes
var (dict, dataMat, sents, w2vMat, nValidSents, labels) = loadMemSentences_CPU(percData, corpus, seed)
// Sizes
// dataMat -> FMat 300 x #sentences
// sents -> SMat 500 x #sentences
// w2vMat -> FMat 300 x #words
// nValidSents -> Int
// labels -> IMat 3 x #sentences

var userDict = loadDict("/home/ana/userDict.sbmat", pad=false);


def query( query_s : String , top : Int, filter: String = null, minWords: Int = 15) = {


  var query_vec = googleVecs(0, ?) * 0;

  var ss = query_s.split(" ");
  var str = "";

  val weights = Array.fill(ss.length+1){1.0}; // Create a weight vector 
  for (i <- 0 until ss.length) {
   str = ss(i).toLowerCase();
   if (str(0) == '[' && str(str.length - 1) == ']') {
      // Convert weight inside the brackets into a double
      weights(i) = (str.stripPrefix("[").stripSuffix("]").trim).toDouble;    
    }  
  }


  var s = "";
  for(i <- 0 until ss.length) {
    s = ss(i).toLowerCase();
   
    if(dict(s) == -1) {
      printf("WARNING: did not find %s in master dict\n", s);
    } else {
      var vec = googleVecs(dict(s), ?);

      if(sum(vec^2)(0) == 0) {
        printf("WARNING: %s is not in google wordvec database\n", s);
      } else {
        printf("adding %s to vector\n", s);
        query_vec += vec * weights(i+1);
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

  var userId = 0;
  var user = "";
  var url = "";

  var i = 0;
  var count = 0;
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

      userId = labels(0,ix);
      user = userDict(userId);
      url = "http://" + user + ".livejournal.com/";

      
      val words = sent.split(" ");
      val numWords = words.length;
      //println(s"Number of words = $numWords");
      if (numWords >= minWords) {
        if (filter == null || !sent.contains(filter)) {
          printf("%.3f -- %-100s -- %s \n", res(ix), sent, url);
          count += 1;
        } 
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
