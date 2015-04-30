import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

import java.io.File
import java.nio.file.{Paths, Files}

import scala.math.log

object preprocessors {

  def preprocess(nrWords:Int,indir:String,outdir:String,masterDict:Dict,sort:Boolean=true,transformation:String="None") {

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

    var map= if (sort) sortdown2(masterDict.counts)._2 else irow(0 until nrWords);
    
    // Save the dictionary in its new order with the preprocessed data
    saveSBMat(outdir+"masterDict.sbmat",SBMat(masterDict.cstr(map)));
    saveDMat(outdir+"masterDict.dmat",masterDict.counts(map));
    
    for ( n <- nrs ) {

      println(s"Currently preprocessing "+n);

      // Build sparse matrix of one hot encoded labels, keeping only the valid rows
      var labels = loadIMat(indir + "data" + n + ".imat");
      var slabels = oneHot(labels(1,?), nrMoods);
      slabels = slabels(validMoodIdx, ?);

      // Build bag of words and truncate
      var bagOfWords = loadSMat(indir + "data" + n + ".smat.lz4");
      if(sort) bagOfWords=bagOfWords(map(0 until nrWords),?) else bagOfWords = bagOfWords(0 until nrWords,?);
      
      if(transformation=="sqrt") bagOfWords.contents(?)=sqrt(bagOfWords);
      if(transformation=="tfidf") {
        nonZeros=find2(bagOfWords>0);
        docFreq+=sum(sparse(nonZeros._1,nonZeros._2,iones(nonZeros._1.length,1),nrWords,bagOfWords.ncols),2);
        ndocs+=bagOfWords.ncols;
      }

      saveSMat(outdir+ "labels" + n + ".smat.lz4", slabels);
      saveSMat(outdir+ "data" + n + ".smat.lz4", bagOfWords);  
      saveSMat(outdir+ "all" + n + ".smat.lz4", slabels on bagOfWords);

    } 
    
    if(transformation=="tfidf") {
      docFreq=log(ndocs)-ln(docFreq);
      for ( n <- nrs ){
        println("Applying tfidf weighting to "+n);
        saveMat(outdir+ "data" + n + ".smat.lz4", loadSMat(outdir+ "data" + n + ".smat.lz4")*@docFreq);  
      }
      saveFMat(outdir+ "idf.fmat",docFreq);
    }

    
    
  } // end def preprocess()

}