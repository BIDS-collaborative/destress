import utils._
import java.io._
import scala.util.Random

tic

// Constants
val corpus = 0;  // 0 - use google corpus, 1 - use LJ corpus
val percData = 0.05;
val sentsSize = 500;  // Size (nrOfWords) of sentences

// Paths to Dictionary, word2VecMatrix, Sentences' Data
val sentsDataDir = "/var/local/destress/featurized_sent/";
val dictFile = "/var/local/destress/tokenized2/masterDict.sbmat";
var w2vMatFile = "/var/local/destress/";
if (corpus == 0)
{
    w2vMatFile += "google_training/wordvec_google_2.fmat";
} else {
    w2vMatFile += "LJ_word2vec/mymat.fmat";
}

// Load Dictionary and w2v
var dict = loadDict(dictFile);
val nrWords = dict.cstr.nrows;
var w2vMat = loadFMat(w2vMatFile);


// Find all the values in filenames dataxxx_sent.smat.lz4
// Get only xxx and then order
val listLabels = new File(sentsDataDir).list.filter(_.endsWith("_sent.smat.lz4")).filter(_.startsWith("data"));
//val numbers =  listLabels.map(c => c.split('.')(0).drop(4).dropRight(5)).sortBy(_.toInt).toList;
val nrDataFiles = listLabels.size;
// SEED Random.setSeed(94720)
val orderFiles2Load = Random.shuffle(listLabels.toList);
val nrFiles2Load = floor(nrDataFiles * percData);



// Init Data Matrices
// Because dataMat and sentsMat are sparse, the best way is to concatenate them
var dataMat = sparse(izeros(0,0), izeros(0,0), izeros(0,0), nrWords, 0);
var sentsMat = sparse(izeros(0,0), izeros(0,0), izeros(0,0), sentsSize, 0);

val tInit = toc
tic

var iter = 0;
var succFiles = 0;
val nrIters = min(nrFiles2Load, nrDataFiles)(0).toInt;
while (iter < nrIters)
{
    println("Iteration %d out of %d" format (iter+1, nrIters));
    val currSentFile = orderFiles2Load(iter);
    
    // Data Files
    // sentences  -> sentsDataDir + "dataxxx_sent.smat.lz4"
    // bagOfWords -> sentsDataDir + "dataxxx.smat.lz4"
    val sentFile = sentsDataDir + currSentFile;
    val bOfwFile = sentsDataDir + currSentFile.dropRight(14)+".smat.lz4";
    if (new File(sentFile).exists() && new File(bOfwFile).exists())
    {
	// Both files exist,  Load data
	val sents = loadSMat(sentFile);
	val data = loadSMat(bOfwFile);	

	if (size(data,2) == size(sents,2))
	{
	    // Data size is consistent, Add to Memory
	    dataMat = dataMat \ data
	    sentsMat = sentsMat \ sents
	    succFiles += 1;
	}

    }

    iter += 1;
}

val tLoad = toc
println("Total Time %f; Load Time = %f secs" format (tInit+tLoad, tLoad));
println("Nr valid Files = %d; Nr of sentences = %d" format(succFiles, size(dataMat,2)));

/**
// QUERY

var magic = data.t * googleVecs;
var lp_max = maxi(magic, 2);

//var n = sum(magic^2, 2);
//var nmagic = magic / sqrt(n);



//for (n <- 1 to 300) {
//  p = (1/n).toDouble;
//  var matReduced = sum(magic^n,2);
//  var lpMagic = Math.pow(matReduced,n);
//}


def query( query_s : String , top : Int) = {

  //var file = "illnessGoogle.txt"
  //val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)));

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

  //var res = nmagic * query_vec.t;
  var res = lp_max * query_vec.t;

  //res(find(n == 0)) = -1; // sentence sums to 0

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
 // for (x <- res_list) {
 //   writer.write(x);
 //}
 // writer.close();
}


// Example usage:
// query("cancer", 20)
// query("amazing", 10)
*/