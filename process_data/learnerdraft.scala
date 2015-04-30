// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import BIDMach.models._
import utils._   // utility methods
import java.io.File
import java.nio.file.{Paths, Files}
import scala.util.Random

// Constants
val nrMoods = 132+1+2;
val nrValidMoods = nrMoods-3;
val validMoodIdx = (1->50) \ (51->94) \ (95->nrMoods) // 0,50,94 aren't valid

// Train Constants
val percTrain = 0.8;
val nFeat = 50000;


// Get listofLabels and BagOfWords - sort 
val indir = "/var/local/destress/featurized2/";
val dicdir = "/var/local/destress/tokenized2/";

val listLabels = new File(indir).list.filter(_.endsWith(".imat")).filter(_.startsWith("data"));
//val listBOW = new File(indir).list.filter(_.endsWith(".smat.lz4")).filter(_.startsWith("data")).sortWith(_<_);
// From list, get the number xx from dataxx.imat in listLabels, sorted (dropRight, then drop)
val nrs = listLabels.map(c => c.split('.')(0)).map(c => c.drop(4)).sortBy(_.toInt); //.sortWith(_<_);
//listLabels.sortBy(_.drop(4).split('.')(0).toInt)
//nrs.map(_.toInt)
//val nn:List[(Int) => String] = (nrs2 => list3)


// Load masterDict
val masterDict = loadDict(dicdir+"masterDict.sbmat", dicdir+"masterDict.dmat");
val nrWords = masterDict.cstr.nrows;

// Set nrBatches
val nrBatches = listLabels.size;
val nrBatches = 10;  // do only these for now

// Read Data Files
var batchIter = 0;
var labels: BIDMat.IMat = izeros(2, 0);
var bagOfWords: BIDMat.SMat = sparse(izeros(nrWords, 0));

while (batchIter < nrBatches) {
      // This is ugly, but only doing it a few times
      // Also, SMat from sparsefile is already a sparse matrix, so we will have concatenations anyway
      // Set up model, to access files directly

      println(s"Batch nr ${batchIter}");
      
      val labelsFile = "data" + nrs(batchIter) + ".imat";
      val sparseFile = "data" + nrs(batchIter) + ".smat.lz4";

      // First check if both files dataxx.imat and dataxx.sbmat.lz4 exist
      if (Files.exists(Paths.get(indir+labelsFile)) && Files.exists(Paths.get(indir+sparseFile))) {
      	 labels \= loadIMat(indir + labelsFile)
	 bagOfWords \= loadSMat(indir + sparseFile)

	// Do Batch Operations Here or outside
      }

      batchIter += 1;
}


// Create Train/Test Sets
val nrDataPts = size(labels,2);
// oneHot -> labelId to SMat, keep validMoodIdx rows
var slabels = oneHot(labels(1,?), nrMoods)
slabels = slabels(validMoodIdx, ?)
// Randomize datapoints
Random.setSeed(1984)
val shuffledIdx = irow(Random.shuffle(List.range(0,nrDataPts))); // indices of datapts shuffled

//Train
val nrTrainPts = IMat(floor(percTrain*nrDataPts))(0);
val data = bagOfWords(0->nFeat, shuffledIdx(0->nrTrainPts));
var cats = full(slabels(?, shuffledIdx(0->nrTrainPts)));
//Get count of train moodIds
var moodCountTrain = sum(cats.t);

// Test
val tdata = bagOfWords(0->nFeat, shuffledIdx(nrTrainPts->nrDataPts));
var tcats = full(slabels(?, shuffledIdx(nrTrainPts->nrDataPts)));  // labels
val tcatsHat = zeros(tcats.nrows, tcats.ncols);  // predictions
var moodCountTest = sum(tcats.t);



// MODEL
val (nn, opts) = GLM.learner(data, cats, GLM.logistic); //GLM.maxp);
//val (nn, opts, tnn, topts) = GLM.learner(data, cats, tdata, tcatsHat, GLM.logistic); //GLM.maxp);
// initializing learner and predictor together makes autoReset = false (by default) for both opts/topts
opts.batchSize = 10000;
opts.npasses = 3;
opts.autoReset = false;
opts.lrate = 1; // default 1
opts.texp = 0.5; // default 0.5
//opts.links = GLM.maxp or 1*iones(nrMoods,1);

// Train and Predict
nn.train

val (tnn, topts) = GLM.predictor(nn.model, tdata, tcatsHat);
topts.autoReset = false; // if this doesn't happen, then nn.modelmat will reset to zeros

tnn.predict
var weights = FMat(nn.modelmat)


// Output Results
val acc = (tcats *@ tcatsHat + (1-tcats) *@ (1-tcatsHat));
val targacc = mean(acc, 2);
val overallAcc = moodCountTest/sum(moodCountTest)*targacc; // overall pred accuracy (targ acc * targ weight)

// Calculate ROC/AUC
val rr = roc2(tcatsHat, full(tcats), 1-full(tcats), 100);
val aucvec = mean(rr);
val (bestv, besti) = maxi2(aucvec);

// Overall (target accuracy * target weight in training)
val overallAUC = moodCountTest/sum(moodCountTest)*aucvec.t;


//Confusion Matrix
var fweights = DMat(weights);
fweights = fweights - mean(fweights, 2);
val wMat = (fweights*fweights.t)*1/nFeat;



val outdir = "/home/anasrferreira/plotdata/";
saveAs(outdir+"wMat.mat", wMat, "wMat")

//saveDMat(outdir+"confMatA.txt", confMatA);
//saveDMat(outdir+"confMatB.txt", confMatB);

saveAs(outdir+"moodCount.mat", moodCountTrain, "moodCountTrain", moodCountTest, "moodCountTest")
//saveIMat(outdir+"moodCount.txt", moodCount);


//val correct = sum(maxi2(tcatsHat)._2 == labels(1,nrTrainPts->nrDataPts))
//val total = nrDataPts-nrTrainPts

//resetGPU; 
//Mat.clearCaches;