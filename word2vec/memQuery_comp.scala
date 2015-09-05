import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

import utils._
import java.io._
import scala.util.Random

object memQuery {

    def getCPUmem() {
    	val mb = 1024*1024;
	val runtime = Runtime.getRuntime;
	println("Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb + " MB.")
	println("Free Memory:  " + runtime.freeMemory / mb + " MB.")
	println("Total Memory: " + runtime.totalMemory / mb + " MB.")
	println("Max Memory:   " + runtime.maxMemory / mb + " MB.")
    }

    def convSent2Vec(w2vMatrix: FMat, sentData: SMat): FMat = {
    	return w2vMatrix*sentData;
    }

    def convSent2VecGPU(w2vMatrix: FMat, sentData: SMat): GMat = {
    	return GMat(w2vMatrix)*GSMat(sentData);
    }


    def loadMemSentences_GPU(percData: Double=0.01, corpus: Int=0): (Dict, FMat, SMat, GMat, Int) =  {

        tic

        // Inputs
        // percData: percentage of data to load in memory
        // corpus: which word2vec model to use // 0 - google; 1 - LJ

	// Constants
	val sentsSize = 500;  // Size (nrOfWords) of sentences

        // Paths to Dictionary, word2VecMatrix, Sentences' Data
        //val sentsDataDir = "/big/livejournal/sentences/"
        //val dictFile = sentsDataDir + "masterDict.sbmat";
        //var w2vMatFile = sentsDataDir;
        //if (corpus == 0) {
        //    w2vMatFile += "googleEmbeddings.fmat";
        //} else {
        //    w2vMatFile += "LJEmbeddings.fmat";
        //}
        val sentsDataDir = "/var/local/destress/featurized_sent/";
        val dictFile = "/var/local/destress/tokenized2/masterDict.sbmat";
        var w2vMatFile = "/var/local/destress/";
        if (corpus == 0) {
           w2vMatFile += "google_training/wordvec_google_2.fmat";
        } else {
           w2vMatFile += "LJ_word2vec/mymat.fmat";
        }


        // Load Dictionary and w2v
        var dict = loadDict(dictFile);
        val nrWords = dict.cstr.nrows;
        var w2vMat = GMat(loadFMat(w2vMatFile).t);  // 300xnrWords
        val sentVecSize = size(w2vMat, 1);


        // Find all the values in filenames dataxxx_sent.smat.lz4
        // Get only xxx and then order
        val listLabels = new File(sentsDataDir).list.filter(_.endsWith("_sent.smat.lz4")).filter(_.startsWith("data"));
        //val numbers =  listLabels.map(c => c.split('.')(0).drop(4).dropRight(5)).sortBy(_.toInt).toList;
        val nrDataFiles = listLabels.size;
	println("Nr of Files: %d" format (nrDataFiles));
        Random.setSeed(94720)  // for repeatability - change later
        val orderFiles2Load = Random.shuffle(listLabels.toList);
        val nrFiles2Load = floor(nrDataFiles * percData)(0).toInt;
	val sentsPerFile = 500000;
	val totalSents = sentsPerFile*nrFiles2Load;

        // Init Data Matrices
        // Because dataMat and sentsMat are sparse, the best way is to concatenate them
        var dataMat = FMat(sentVecSize, totalSents);
        var sentsMat = sparse(izeros(0,0), izeros(0,0), izeros(0,0), sentsSize, 0);

        val tInit = toc
        tic

        var iter = 0;
        var succFiles = 0;
        val nrIters = min(nrFiles2Load, nrDataFiles)(0);
	var sentsCount = 0;

        while (iter < nrIters) {
            println("Iteration %d out of %d" format (iter+1, nrIters));
            val currSentFile = orderFiles2Load(iter);

            // Data Files
            // sentences  -> sentsDataDir + "dataxxx_sent.smat.lz4"
            // bagOfWords -> sentsDataDir + "dataxxx.smat.lz4"
            val sentFile = sentsDataDir + currSentFile;
            val bOfwFile = sentsDataDir + currSentFile.dropRight(14)+".smat.lz4";
            if (new File(sentFile).exists() && new File(bOfwFile).exists()) {
                // Both files exist,  Load data
                val sents = loadSMat(sentFile);
                val data = GSMat(loadSMat(bOfwFile));

                if (size(data,2) == size(sents,2)) {
                    // Data size is consistent, Add to Memory

                        // Process each sentence data into vector
                        val nrSents = size(data, 2)
			val nrSents2Proc = min(nrSents, totalSents-sentsCount)(0);

			println("%d %d %d" format (sentsCount, sentsCount+nrSents2Proc, nrSents));
			if (nrSents2Proc == nrSents) {
			    val tempProd = w2vMat*data;			    
			    dataMat(?, sentsCount->(sentsCount+nrSents)) = FMat(tempProd);
			    tempProd.free;
			    sentsMat \= sents;
			}
			else {
			   val tempMat = SMat(data);
			   val temp2 = GSMat(tempMat(?, 0->nrSents2Proc));
			   val tempProd = w2vMat*temp2;
			   dataMat(?, sentsCount->(sentsCount+nrSents2Proc)) = FMat(tempProd);
			   temp2.free;
			   tempProd.free
			   sentsMat \= sents(?, 0->nrSents2Proc);
			}

			data.free;
			Mat.clearCaches;
			sentsCount += nrSents2Proc;
			if (sentsCount >= totalSents) {
			   iter = nrIters; // breaks the loop
			}
                        succFiles += 1;
                    }
            }

            iter += 1;
        }

        val tLoad = toc;

        println("Total Time %f; Load Time = %f secs" format (tInit+tLoad, tLoad));
        println("Nr valid Files = %d; Nr of sentences = %d" format(succFiles, size(dataMat,2)));

        return (dict, dataMat, sentsMat, w2vMat, sentsCount)
    }



def loadMemSentences_CPU(percData: Double=0.01, corpus: Int=0): (Dict, FMat, SMat, FMat, Int) =  {

        tic

        // Inputs
        // percData: percentage of data to load in memory
        // corpus: which word2vec model to use // 0 - google; 1 - LJ

        // Constants
        val sentsSize = 500;  // Size (nrOfWords) of sentences

        // Paths to Dictionary, word2VecMatrix, Sentences' Data
        val sentsDataDir = "/big/livejournal/sentences/"
        val dictFile = sentsDataDir + "masterDict.sbmat";
        var w2vMatFile = sentsDataDir;
        if (corpus == 0) {
            w2vMatFile += "googleEmbeddings.fmat";
        } else {
            w2vMatFile += "LJEmbeddings.fmat";
        }
        //val sentsDataDir = "/var/local/destress/featurized_sent/";
        //val dictFile = "/var/local/destress/tokenized2/masterDict.sbmat";
        //var w2vMatFile = "/var/local/destress/";
        //if (corpus == 0) {
        //   w2vMatFile += "google_training/wordvec_google_2.fmat";
        //} else {
        //   w2vMatFile += "LJ_word2vec/mymat.fmat";
        //}

        // Load Dictionary and w2v
        var dict = loadDict(dictFile);
        val nrWords = dict.cstr.nrows;
        var w2vMat = loadFMat(w2vMatFile).t;  // 300xnrWords
        val sentVecSize = size(w2vMat, 1);


        // Find all the values in filenames dataxxx_sent.smat.lz4
        // Get only xxx and then order
        val listLabels = new File(sentsDataDir).list.filter(_.endsWith("_sent.smat.lz4")).filter(_.startsWith("data"));
        //val numbers =  listLabels.map(c => c.split('.')(0).drop(4).dropRight(5)).sortBy(_.toInt).toList;
        val nrDataFiles = listLabels.size;
        println("Nr of Files: %d" format (nrDataFiles));
        Random.setSeed(94720)  // for repeatability - change later
        val orderFiles2Load = Random.shuffle(listLabels.toList);
        val nrFiles2Load = floor(nrDataFiles * percData)(0).toInt;
        val sentsPerFile = 500000;
        val totalSents = sentsPerFile*nrFiles2Load;

        // Init Data Matrices
        // Because dataMat and sentsMat are sparse, the best way is to concatenate them
        var dataMat = FMat(sentVecSize, totalSents);
        var sentsMat = sparse(izeros(0,0), izeros(0,0), izeros(0,0), sentsSize, 0);

        val tInit = toc
        tic

        var iter = 0;
        var succFiles = 0;
        val nrIters = min(nrFiles2Load, nrDataFiles)(0);
        var sentsCount = 0;

        while (iter < nrIters) {
            println("Iteration %d out of %d" format (iter+1, nrIters));
            val currSentFile = orderFiles2Load(iter);

            // Data Files
            // sentences  -> sentsDataDir + "dataxxx_sent.smat.lz4"
            // bagOfWords -> sentsDataDir + "dataxxx.smat.lz4"
            val sentFile = sentsDataDir + currSentFile;
            val bOfwFile = sentsDataDir + currSentFile.dropRight(14)+".smat.lz4";
            if (new File(sentFile).exists() && new File(bOfwFile).exists()) {
                // Both files exist,  Load data
                val sents = loadSMat(sentFile);
                val data = loadSMat(bOfwFile);

                if (size(data,2) == size(sents,2)) {
                    // Data size is consistent, Add to Memory

                        // Process each sentence data into vector
                        val nrSents = size(data, 2)
                        val nrSents2Proc = min(nrSents, totalSents-sentsCount)(0);

                        println("%d %d %d" format (sentsCount, sentsCount+nrSents2Proc, nrSents));
                        if (nrSents2Proc == nrSents) {
                            dataMat(?, sentsCount->(sentsCount+nrSents)) = w2vMat*data;
                            sentsMat \= sents;
                        }
                        else {
                           dataMat(?, sentsCount->(sentsCount+nrSents2Proc)) = w2vMat*data(?, 0->nrSents2Proc);
                           sentsMat \= sents(?, 0->nrSents2Proc);
                        }

                        sentsCount += nrSents2Proc;
                        if (sentsCount >= totalSents) {
                           iter = nrIters; // breaks the loop
                        }
                        succFiles += 1;
                    }
            }

            iter += 1;
        }

        val tLoad = toc;

        println("Total Time %f; Load Time = %f secs" format (tInit+tLoad, tLoad));
        println("Nr valid Files = %d; Nr of sentences = %d" format(succFiles, size(dataMat,2)));

        return (dict, dataMat, sentsMat, w2vMat, sentsCount)
    }



def loadMemSentences_CPUarray(percData: Double=0.01, corpus: Int=0): (Dict, FMat, SMat, FMat, Int) =  {

        tic

        // Inputs
        // percData: percentage of data to load in memory
        // corpus: which word2vec model to use // 0 - google; 1 - LJ

        // Constants
        val sentsSize = 500;  // Size (nrOfWords) of sentences
	val MaxSentsPerMat = 5500000; 

        // Paths to Dictionary, word2VecMatrix, Sentences' Data
        val sentsDataDir = "/big/livejournal/sentences/"
        val dictFile = sentsDataDir + "masterDict.sbmat";
        var w2vMatFile = sentsDataDir;
        if (corpus == 0) {
            w2vMatFile += "googleEmbeddings.fmat";
        } else {
            w2vMatFile += "LJEmbeddings.fmat";
        }
        //val sentsDataDir = "/var/local/destress/featurized_sent/";
        //val dictFile = "/var/local/destress/tokenized2/masterDict.sbmat";
        //var w2vMatFile = "/var/local/destress/";
        //if (corpus == 0) {
        //   w2vMatFile += "google_training/wordvec_google_2.fmat";
        //} else {
        //   w2vMatFile += "LJ_word2vec/mymat.fmat";
        //}

        // Load Dictionary and w2v
        var dict = loadDict(dictFile);
        val nrWords = dict.cstr.nrows;
        var w2vMat = loadFMat(w2vMatFile).t;  // 300xnrWords
        val sentVecSize = size(w2vMat, 1);


        // Find all the values in filenames dataxxx_sent.smat.lz4
        // Get only xxx and then order
        val listLabels = new File(sentsDataDir).list.filter(_.endsWith("_sent.smat.lz4")).filter(_.startsWith("data"));
        //val numbers =  listLabels.map(c => c.split('.')(0).drop(4).dropRight(5)).sortBy(_.toInt).toList;
        val nrDataFiles = listLabels.size;
        println("Nr of Files: %d" format (nrDataFiles));
        Random.setSeed(94720)  // for repeatability - change later
        val orderFiles2Load = Random.shuffle(listLabels.toList);
        val nrFiles2Load = floor(nrDataFiles * percData)(0).toInt;
        val sentsPerFile = 500000;
        val totalSents = sentsPerFile*nrFiles2Load;

        // Init Data Matrices
        // Because dataMat and sentsMat are sparse, the best way is to concatenate them
        var dataMat = FMat(sentVecSize, totalSents);
        var sentsMat = sparse(izeros(0,0), izeros(0,0), izeros(0,0), sentsSize, 0);

        val tInit = toc
        tic

        var iter = 0;
        var succFiles = 0;
        val nrIters = min(nrFiles2Load, nrDataFiles)(0);
        var sentsCount = 0;

        while (iter < nrIters) {
            println("Iteration %d out of %d" format (iter+1, nrIters));
            val currSentFile = orderFiles2Load(iter);

            // Data Files
            // sentences  -> sentsDataDir + "dataxxx_sent.smat.lz4"
            // bagOfWords -> sentsDataDir + "dataxxx.smat.lz4"
            val sentFile = sentsDataDir + currSentFile;
            val bOfwFile = sentsDataDir + currSentFile.dropRight(14)+".smat.lz4";
            if (new File(sentFile).exists() && new File(bOfwFile).exists()) {
                // Both files exist,  Load data
                val sents = loadSMat(sentFile);
                val data = loadSMat(bOfwFile);

                if (size(data,2) == size(sents,2)) {
                    // Data size is consistent, Add to Memory

                        // Process each sentence data into vector
                        val nrSents = size(data, 2)
                        val nrSents2Proc = min(nrSents, totalSents-sentsCount)(0);

                        println("%d %d %d" format (sentsCount, sentsCount+nrSents2Proc, nrSents));
                        if (nrSents2Proc == nrSents) {
                            dataMat(?, sentsCount->(sentsCount+nrSents)) = w2vMat*data;
                            sentsMat \= sents;
                        }
                        else {
                           dataMat(?, sentsCount->(sentsCount+nrSents2Proc)) = w2vMat*data(?, 0->nrSents2Proc);
                           sentsMat \= sents(?, 0->nrSents2Proc);
                        }

                        sentsCount += nrSents2Proc;
                        if (sentsCount >= totalSents) {
                           iter = nrIters; // breaks the loop
                        }
                        succFiles += 1;
                    }
            }

            iter += 1;
        }

        val tLoad = toc;

        println("Total Time %f; Load Time = %f secs" format (tInit+tLoad, tLoad));
        println("Nr valid Files = %d; Nr of sentences = %d" format(succFiles, size(dataMat,2)));

        return (dict, dataMat, sentsMat, w2vMat, sentsCount)
    }
}

/**
// QUERY






var magic = data.t * w2vMat;
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

  var query_vec = w2vMat(0, ?) * 0;

  var ss = query_s.split(" ");
  var s = "";

  for(x <- ss) {
    s = x.toLowerCase();

    if(dict(s) == -1) {
      printf("WARNING: did not find %s in master dict\n", s);
    } else {
      var vec = w2vMat(dict(s), ?);

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