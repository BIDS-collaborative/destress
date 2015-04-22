import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

import java.io.File
import java.nio.file.{Paths, Files}

object preprocessors {

  def preprocess(nrWords:Int,indir:String,outdir:String,masterDict:Dict,sort:Boolean=true) {

    // Constants
    val nrMoods = 132+1+2;
    val nrValidMoods = nrMoods-3;
    val validMoodIdx = (1->50) \ (51->94) \ (95->nrMoods); // 0,50,94 aren't valid

    // Load the filenames of all the imats in indir
    val listLabels = new File(indir).list.filter(_.endsWith(".imat")).filter(_.startsWith("data"));
    // From list, get the number xx from dataxx.imat in listLabels, sorted (dropRight, then drop)
    val nrs = listLabels.map(c => c.split('.')(0)).map(c => c.drop(4)).sortBy(_.toInt); //.sortWith(_<_);
    
    var map = sortdown2(masterDict.counts)._2;
    
    for ( n <- nrs ) {
      
      println(s"Currently preprocessing "+n);

      val labelsFile = "data" + n + ".imat";
      val sparseFile = "data" + n + ".smat.lz4";

      // Build sparse matrix of one hot encoded labels, keeping only the valid rows
      var  labels = loadIMat(indir + "data" + n + ".imat");
      var slabels = oneHot(labels(1,?), nrMoods);
      slabels = slabels(validMoodIdx, ?);
      
      // Build bag of words and truncate
      var  bagOfWords = loadSMat(indir + "data" + n + ".smat.lz4");
      if(sort) bagOfWords=bagOfWords(map(0 until nrWords),?) else bagOfWords = bagOfWords(0 until nrWords,?);

      saveSMat(outdir+ "labels" + n + ".smat.lz4", slabels);
      saveSMat(outdir+ "data" + n + ".smat.lz4", bagOfWords);  
      saveSMat(outdir+ "all" + n + ".smat.lz4", slabels on bagOfWords);

    }


  }

}