import utils._


var dict = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");
var data = loadSMat("/var/local/destress/featurized_sent/data1.smat.lz4");
var sents = loadSMat("/var/local/destress/featurized_sent/data1_sent.smat.lz4")
//var googleVecs = loadFMat("/var/local/destress/google_training/wordvec_google_2.fmat")
var liveJournalVecs = loadFMat("/var/local/destress/LJ_word2vec/mymat.fmat")

var userDict = loadDict("/home/pierre/combined/userDict.sbmat", pad=false);
var labels = loadIMat("/var/local/destress/featurized_sent/data1.imat");

//var magic = data.t * googleVecs;
var magic = data.t * liveJournalVecs;
var n = sum(magic^2, 2);
var nmagic = magic / sqrt(n);


var illness_google = Array(
  "illness",
  "my mother who fell ill to a mental illness twice",
  "my brother is sick with bronchitis",
  "conjunctivitis and an ear infection",
  "i have had pneumonia",
  "my father was diagnosed with cancer in early june lung cancer that has spread to the bones",
  "tess suffering from her cancer scare is causing me too much pain already",
  "melz got the flu",
  "i may have acute bronchitis , which could lead to asthma",
  "fucking sickness !",
  "i have been diagnosed with clinical depression",
  "my mom is in the hospital with a kidney infection",
  "i probably have a sinus infection",
  "she goes through all this depression since her cancer operations");

//var illness_lj = Array( 
//  
//);



def make_query_vec(query_s : String) : FMat  = {

//  var query_vec = googleVecs(0, ?) * 0;

  var query_vec = liveJournalVecs(0, ?) * 0;
  var ss = query_s.split(" ");
  var s = "";

  for(x <- ss) {
    s = x.toLowerCase();

    if(dict(s) == -1) {
      // printf("WARNING: did not find %s in master dict\n", s);
    } else {
      //var vec = googleVecs(dict(s), ?);
      var vec = liveJournalVecs(dict(s), ?);
      
      if(sum(vec^2)(0) == 0) {
        // printf("WARNING: %s is not in google wordvec database\n", s);
      } else {
        // printf("adding %s to vector\n", s);
        query_vec += vec;
      }
    }
  }
  query_vec = query_vec / sqrt(sum(query_vec^2));

  return(query_vec);
}


def multi_query(queries : Array[String], top : Int, report:Boolean = true, ignore:Int = 0) : Array[String] =  {
  var res = zeros(data.ncols, 1);

  var i = 0;

  while(i < queries.length) {
    var query_vec = make_query_vec(queries(i));

    var new_res = nmagic * query_vec.t;
    new_res(find(n == 0)) = 0; // deal with sentences that sums to 0

    res = res + new_res;

    i += 1;
  }

  var (_, bestIndex) = sortdown2(res);

  var nwords = size(sents)(0);
  var prev = "   ";
  var prev_res = -1f;

  var userId = 0;
  var user = "";
  var url = "";

  i = 0;

  var out = new Array[String](top-ignore);

  var count = 0;
  
  while(count < top) {
    var ix = bestIndex(i);
    var curr = IMat(FMat(sents(find(sents(?, ix) != 0), ix)));
    var z = dict(curr).t;
    var sent = (z ** csrow(" ")).toString().replace(" ,", " ");

    if(res(ix) != prev_res) {
      prev = sent;
      prev_res = res(ix);

      userId = labels(0,ix);
      user = userDict(userId);
      url = "http://" + user + ".livejournal.com/";

      if(count >= ignore) {
        out(count - ignore) = sent;
      }

      if(report) {
        printf("%.3f -- %-100s -- %s \n", res(ix), sent, url);
      }

      count += 1;
    }

    i += 1;
  }

  if(report) {
    println();
  }

  return out;
}


def query( query_s : String , top : Int) = {

  var query_vec = make_query_vec(query_s);

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
      count += 1;
    }
    // else {
    //   printf("ignoring %s\n", sent);
    // }
    i += 1;
  }
  println();
}

// Example usage:
// multi_query(illness_queries, 100)
// multi_query(Array("drunk", "sad"), 20)


