// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._   // utility methods
import java.io.File

val indir = "/var/local/destress/featurized/";
val dicdir = "/var/local/destress/tokenized/";

val listLabels = new File(indir).list.filter(_.endsWith(".imat"));
val listBOW = new File(indir).list.filter(_.endsWith(".smat.lz4"));

//Not needed for now
val masterDict = loadDict(dicdir+"masterDict.sbmat", dicdir+"masterDict.dmat");
val nrWords = masterDict.cstr.nrows;

val nrBatches = listLabels.size;
val nrBatches = 1;  // do only one for now

var batchIter = 0;

var labels: BIDMat.IMat = izeros(2,0);  // declared here to have scope outside of loop
var bagOfWords: BIDMat.SMat = sparse(izeros(nrWords,0));

while (batchIter < nrBatches) {

	labels \= loadIMat(indir + listLabels(batchIter)) // in future, probably won't want to concatenate
	bagOfWords \= loadSMat(indir + listBOW(batchIter))


	// Do Batch Operations Here


	batchIter += 1;
}

// Or For now just concat everything
// And do things HERE
