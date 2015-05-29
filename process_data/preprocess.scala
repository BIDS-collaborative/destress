import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

import java.io.File
import java.nio.file.{Paths, Files}

import scala.math.log

object preprocessors {

  def preprocess(nrWords:Int,indir:String,outdir:String,masterDict:Dict,testPercent:Float=0.0f,sort:Boolean=true,transformation:String="none",labelTransf:Mat=null) {

    /** This function preprocesses raw bag-of-words data points from featurizeMoodID
     *  
     *  nrWords: Number of words from dictionary to use in training
     *  indir: Folder containing files in the format data###.imat and data###.smat, e.g. data001.imat
     *  outdir: Folder to save preprocessed train and test data
     *  masterDict: Dictionary corresponding to the rows of featurized smats, including counts to sort by if desired
     *  testPercent: Fraction of data to reserve as a test set, randomly sampling from each input file.
     *  sort: If true then resort the words by the counts in masterDict before truncating. Default is 
     *  transformation: "none" - Does no additional processing on data
     *                  "sqrt" - Takes the sqrt of the frequencies in the bag of words
     *                  "tfidf"- Performs tfidf weighting using idf calculated on training data only. idf is give by log N-log docFreq
     *                             where N is the number of training posts and docFreq is the number of training posts where a word occurs. 
     */
    
    // Might want to make this an argument and/or allow different sizes for test and train
    val batchSize=100000;
    var trainBatchNumber=0;
    var testBatchNumber=0;
    
    // Constants
    val nrMoods = 132+1+2;
    val nrValidMoods = nrMoods-3;
    val validMoodIdx = (1->50) \ (51->94) \ (95->nrMoods); // 0,50,94 aren't valid
    
    // tfidf trackers
    var ndocs:Double=0;
    var docFreq=zeros(nrWords,1);
    var nonZeros:(IMat,IMat)=(null,null); 

    // Load the filenames of all the imats in indir
    val listLabels = new File(indir).list.filter(_.endsWith(".imat")).filter(_.startsWith("data"));
    // From list, get the number xx from dataxx.imat in listLabels, sorted (dropRight, then drop)
    val nrs = listLabels.map(c => c.split('.')(0)).map(c => c.drop(4)).sortBy(_.toInt); //.sortWith(_<_);

    var map= if (sort) sortdown2(masterDict.counts)._2(0 until nrWords) else irow(0 until nrWords);
    
    // Save the dictionary in its new order with the preprocessed data
    saveSBMat(outdir+"masterDict.sbmat",SBMat(masterDict.cstr(map)));
    saveDMat(outdir+"masterDict.dmat",masterDict.counts(map));
    
    // Buffers of test and train data. This is not a very efficient implementation
    var testData = sparse(izeros(0,0),izeros(0,0),izeros(0,0),nrWords,0);
    var testLabels = if(labelTransf==null) sparse(izeros(0,0),izeros(0,0),izeros(0,0),nrValidMoods,0) else sparse(izeros(0,0),izeros(0,0),izeros(0,0),labelTransf.nrows,0);
    var trainData = sparse(izeros(0,0),izeros(0,0),izeros(0,0),nrWords,0);
    var trainLabels = if(labelTransf==null) sparse(izeros(0,0),izeros(0,0),izeros(0,0),nrValidMoods,0) else sparse(izeros(0,0),izeros(0,0),izeros(0,0),labelTransf.nrows,0);
    
    for ( n <- nrs ) {

      println(s"Currently preprocessing "+n);

      // Build sparse matrix of one hot encoded labels, keeping only the valid rows
      var labels = loadIMat(indir + "data" + n + ".imat");
      var slabels = oneHot(labels(1,?), nrMoods);

      if(labelTransf==null) slabels = slabels(validMoodIdx, ?) 
      else if (labelTransf.ncols==nrMoods) {
        slabels=SMat(labelTransf)*slabels;
      }
      else if (labelTransf.ncols==nrValidMoods) {
        slabels = slabels(validMoodIdx, ?);
        slabels = SMat(labelTransf)*slabels;
      }
      else println("ERRROR: INVALID LABEL TRANSFORMATION MATRIX");
      
      // Build bag of words and truncate
      var bagOfWords = loadSMat(indir + "data" + n + ".smat.lz4")(map,?);
      
      // Split train and test data. Should these be sorted after splitting to improve sparsification speed?
      val randIdx=randperm(labels.ncols); 
      val testIdx=randIdx(0 until ceil(labels.ncols*testPercent)(0).toInt);
      val trainIdx=randIdx( ceil(labels.ncols*testPercent)(0).toInt until labels.ncols );
      
      if(transformation=="sqrt") bagOfWords.contents(?)=sqrt(bagOfWords);
      if(transformation=="tfidf") {
        // Get idf weights from train data only
        nonZeros=find2(bagOfWords(?,testIdx)>0);
        docFreq+=accum(nonZeros._1,iones(nonZeros._1.length,1),nrWords,1);
        ndocs+=trainIdx.length;
      }
      
      // Concatenate the test and train data to buffers
      trainData=trainData \ bagOfWords(?,trainIdx);
      trainLabels=trainLabels \ slabels(?,trainIdx);
      
      testData=testData \ bagOfWords(?,testIdx);
      testLabels=testLabels \ slabels(?,testIdx);

      // Save a train batch if the buffer is big enough
      while (trainData.ncols>batchSize) {
        saveFMat(outdir+ "trainLabels" + f"$trainBatchNumber%03d" + ".fmat.lz4", full(trainLabels(?,0 until batchSize)));
        saveSMat(outdir+ "trainData" + f"$trainBatchNumber%03d" + ".smat.lz4", trainData(?,0 until batchSize));  
        //saveSMat(outdir+ "train" + f"$trainBatchNumber%03d" + ".smat.lz4", trainLabels(0 until batchSize,?) on trainData(0 until batchSize,?));
        
        trainLabels = trainLabels(?,batchSize until trainLabels.ncols);
        trainData = trainData(?,batchSize until trainData.ncols);
        
        trainBatchNumber+=1;
      }
      
      // Save a test batch if the buffer is big enough
      while (testData.ncols>batchSize) {
        saveFMat(outdir+ "testLabels" + f"$testBatchNumber%03d" + ".fmat.lz4", full(testLabels(?,0 until batchSize)));
        saveSMat(outdir+ "testData" + f"$testBatchNumber%03d" + ".smat.lz4", testData(?,0 until batchSize));  
        //saveSMat(outdir+ "test" + f"$testBatchNumber%03d" + ".smat.lz4", testLabels(0 until batchSize,?) on testData(0 until batchSize,?));
        
        testLabels = testLabels(?,batchSize until testLabels.ncols);
        testData = testData(?,batchSize until testData.ncols);
        
        testBatchNumber+=1;
      }

    } 
    
    // Save leftovers, if any
    if (trainLabels.ncols!=0) {
      saveFMat(outdir+ "trainLabels" + f"$trainBatchNumber%03d" + ".fmat.lz4", full(trainLabels));
      saveSMat(outdir+ "trainData" + f"$trainBatchNumber%03d" + ".smat.lz4", trainData); 
      //saveSMat(outdir+ "train" + f"$trainBatchNumber%03d" + ".smat.lz4", trainLabels on trainData);
    } else trainBatchNumber-=1;
    
    if (testLabels.ncols!=0) {
      saveFMat(outdir+ "testLabels" + f"$testBatchNumber%03d" + ".fmat.lz4", full(testLabels));
      saveSMat(outdir+ "testData" + f"$testBatchNumber%03d" + ".smat.lz4", testData);  
      //saveSMat(outdir+ "test" + f"$testBatchNumber%03d" + ".smat.lz4", testLabels on testData);
    } else testBatchNumber-=1;
    
    // Reprocess files with tfidf transformation if selected
    if(transformation=="tfidf") {
      val nonZeroIdx=find(docFreq!=0);
      docFreq(nonZeroIdx)=log(ndocs)-ln(docFreq(nonZeroIdx));
      saveFMat(outdir+ "idf.fmat",docFreq);
      for ( n <- 0 to trainBatchNumber ){
        println("Applying tfidf weighting to training batch "+n);
        saveMat(outdir+ "trainData" + f"$n%03d" + ".smat.lz4", loadSMat(outdir+ "trainData" + f"$n%03d" + ".smat.lz4")*@docFreq);  
      }
      for ( n <- 0 to trainBatchNumber ){
        println("Applying tfidf weighting to test batch "+n);
        saveMat(outdir+ "testData" + f"$n%03d" + ".smat.lz4", loadSMat(outdir+ "testData" + f"$n%03d" + ".smat.lz4")*@docFreq);  
      }

    } // end if(transformation=="tfidf")

  } // end def preprocess()

}