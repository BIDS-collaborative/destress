import utils._
import memQuery._
import java.io._
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

var corpus = 0;  // select 1-for LJ; 0-for Google

val resultsDir = "/big/livejournal/full_results/user_studies/";
val sentsDataDir = "/big/livejournal/sentences/";

val nFiles = 1130; // all of them, except last one because Pierre messed up
// val nFiles = 10; 


val w2vMatFile = sentsDataDir + "googleEmbeddings.fmat";
// val w2vMatFile = sentsDataDir + "LJEmbeddings.fmat";

val dictFile = sentsDataDir + "masterDict.sbmat";

var dict = loadDict(dictFile);
var w2vMat = loadFMat(w2vMatFile).t;  // 300xnrWords

def make_query_vec( queries : Array[String]) : FMat = {

  var query_vec = FMat(size(w2vMat, 1), 1);

  for(query_s <- queries) {

    // Converts input query to dictionary indexes
    var ss = query_s.toLowerCase().split(" ")

    var str = "";

    val weights = Array.fill(ss.length+1){1.0}; // Create a weight vector
    for (i <- 0 until ss.length) {
      str = ss(i).toLowerCase();
      if (str(0) == '[' && str(str.length - 1) == ']') {
        // Convert weight inside the brackets into a double
        weights(i) = (str.stripPrefix("[").stripSuffix("]").trim).toDouble;
        ss(i) = null;
      }
    }

    // Convert input query to a word2vec vector
    var s = "";
    for(i <- 0 until ss.length) {
      if(ss(i) != null) {
        s = ss(i).toLowerCase();

        if(dict(s) == -1) {
          printf("WARNING: did not find %s in master dict\n", s);
        } else {
          var vec = w2vMat(?, dict(s));

          if(sum(vec^2)(0) == 0) {
            // printf("WARNING: %s is not in google wordvec database\n", s);
          } else {
            // printf("adding %s to vector\n", s);
            query_vec += vec * weights(i+1);
          }
        }
      }
    }

  }
  // Normalize
  // size 300x1
  query_vec = query_vec / norm(query_vec);

  return(query_vec);
}

// need to load query vectors here
// for now, let's pretend we have two
// var vec1 = make_query_vec(Array("i feel fantastic [10]"));
// var vec2 = make_query_vec(Array("my mom is smart"));
// var vecs = Array(vec1, vec2);

var svecs = Array(
  // Airi
  Array("neighbor"),
  Array("neighbor in my building"),
  Array("next door neighbor"),
  Array("well ... last night was uneventful ... really got to know the next door neighbor ",
    "saturday got up kinda early and went over the next door neighbor s house so i could meet cody and kayla"),
  Array("really got to know the next door neighbor ", "went over next door neighbor house"),
  Array("really got know next door neighbor", "next door neighbor house"),
  Array("my next door neighbor was screaming you like fucking with my stuff"),
  Array("the annoying neighbor from next door , jason , is moving out !"),

  // Coye
  Array("trust and emotion"),
  Array("exchange reciprocity"),
  Array("exchange reciprocity negotiation"),
  Array("personality and a mutual respect"),
  Array("family"),
  Array("holiday emotions"),
  Array("the night somehow fules my creativity and fills me with ideas and thoughts",
    "my feelings are actually always repressed ... i tend not to show them sometimes",
    "many fears are born of fatigue and loneliness"),

  // Victor
  Array("couple obese"),
  Array("eating obese"),
  Array("i m trying to eat healthy and avoid eating sweets"),
  Array("i lost weight and kept it off"),
  Array("i lost weight by using calorie counting"),
  Array("this other diet is better than calorie counting"),
  Array("I redesigned my lifestyle"),
  Array("I redesigned my lifestyle to lose weight"),
  Array("gobble eat devour"),
  Array("gobble devour"),
  Array("devour gobble ingest"),
  Array("slow carb diet"),
  Array("i am following atkins diet"),
  Array("i follow the paleo diet"),
  Array("yoyo diet"),
  Array("yo-yo diet"),
  Array("today begins a healthy eating and exercise regime"),
  Array("balancing better exercise and eating habits with my much loved unhealthy habits")

)

var vecs = new Array[FMat](svecs.length);


// get top 1000 queries from each file
var top = 1000;

// min words is 15
var minWords = 15;

// define filters here
// in this case, sentences with "fantastic" and "feel" are excluded
// var filters = Array[String]("fantastic", null);

// no filters
var filters = Array.fill[String](vecs.length){null}


var filterRegex = new Array[Regex](vecs.length);

var outSents = new Array[ListBuffer[String]](vecs.length);
var outRes = new Array[ListBuffer[Float]](vecs.length);

for(i <- 0 until vecs.length) {
  vecs(i) = make_query_vec(svecs(i))

  if(filters(i) == null) {
    filterRegex(i) = null;
  } else {
    filterRegex(i) = filters(i).r;
  }

  outSents(i) = ListBuffer();
  outRes(i) = ListBuffer();
}

tic;
println("");

// loop through list of files
for(fnum <- 1 to nFiles) {
  println(fnum);

  // load the data
  var sentFile = sentsDataDir + "data" + fnum + "_sent.smat.lz4";
  var bowFile = sentsDataDir + "data" + fnum + ".smat.lz4";
  val idFile = sentsDataDir + "data" + fnum + ".imat";

  var sents = loadSMat(sentFile);
  var data = loadSMat(bowFile);
  var ids = loadIMat(idFile);

  var magic = w2vMat*data;
  magic ~ magic / sqrt(magic dot magic)


  for(vnum <- 0 until outSents.length) {

    // perform query
    var query_vec = vecs(vnum);

    var res = query_vec.t * magic;  // 1x#sentences
    res(find(1-((res dot res)>=0))) = -1; // sentence sums to 0

    // Sort Results to Return Top Ones
    var (x, bestIndex) = sortdown2(res);

    var prev_res = -1f;

    var i = 0
    var count = 0;

    while(count < top && i < res.length) {
      var ix = bestIndex(i);

      var curr = IMat(FMat(sents(find(sents(?, ix)), ix)));
      var z = dict(curr).t;
      var sent = (z ** csrow(" ")).toString().replace(" ,", " ");
      var numWords = z.length;


      if(res(ix) != prev_res && // discard repeated strings?
        numWords >= minWords && // minimum words
        (filterRegex(vnum)== null || filterRegex(vnum).findFirstIn(sent) == None) // filter for words
      ) {
        prev_res = res(ix);

        // printf("%.3f -- %s\n", res(ix), sent);

        outSents(vnum).append(sent);
        outRes(vnum).append(res(ix));
        count += 1;
      }

      i += 1;

    }
  }

}

println("it took " + toc + " seconds to run on full data");

tic;

println("saving results...")
// save results
for(i <- 0 until vecs.length) {
  var res = row(outRes(i).toList);
  saveFMat(resultsDir + "res_" + i + ".fmat", res);

  var sents = csrow(outSents(i).toList);
  saveCSMat(resultsDir + "sents_" + i + ".csmat.lz4", sents);
}

println("it took " + toc + " seconds to save data");

println("done!")
