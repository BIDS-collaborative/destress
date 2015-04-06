// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._   // utility methods
import java.io.File
import java.nio.file.{Paths, Files}

val indir = "/var/local/destress/featurized/";
val dicdir = "/var/local/destress/tokenized/";

// Get listofLabels and BagOfWords - sort 
val listLabels = new File(indir).list.filter(_.endsWith(".imat")).filter(_.startsWith("data"));
//val listBOW = new File(indir).list.filter(_.endsWith(".smat.lz4")).filter(_.startsWith("data")).sortWith(_<_);
// From list, get the number xx from dataxx.imat in listLabels, sorted (dropRight, then drop)
val nrs = listLabels.map(c => c.split('.')(0)).map(c => c.drop(4)).sortBy(_.toInt); //.sortWith(_<_);



//Not needed for now
val masterDict = loadDict(dicdir+"masterDict.sbmat", dicdir+"masterDict.dmat");
val nrWords = masterDict.cstr.nrows;

val nrBatches = listLabels.size;
val nrBatches = 100;  // do only one for now

var batchIter = 0;

var labels: BIDMat.IMat = izeros(2,0);  // declared here to have scope outside of loop
var bagOfWords: BIDMat.SMat = sparse(izeros(nrWords,0));

while (batchIter < nrBatches) {

      println(s"Batch nr ${batchIter}");
      
      val labelsFile = "data" + nrs(batchIter) + ".imat";
      val sparseFile = "data" + nrs(batchIter) + ".smat.lz4";

      // First check if both files dataxx.imat and dataxx.sbmat.lz4 exist
      if (Files.exists(Paths.get(indir+labelsFile)) && Files.exists(Paths.get(indir+sparseFile))) {
      	 //HERE - data.sbmat.lz4 from featurizer needs fixing
      	 labels \= loadIMat(indir + labelsFile) // in future, probably won't want to concatenate
	 bagOfWords \= loadSMat(indir + sparseFile)

	// Do Batch Operations Here
      }

      batchIter += 1;
}

// Or For now just concat everything
// And do things HERE
