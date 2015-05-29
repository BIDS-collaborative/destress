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
val percTrain = 0.9;


// Get listofLabels and BagOfWords - sort 
val indir = "/var/local/destress/preprocessed/";
val dicdir = "/var/local/destress/tokenized2/";

val listLabels = new File(indir).list.filter(_.endsWith(".smat.lz4")).filter(_.startsWith("data"));
//val listBOW = new File(indir).list.filter(_.endsWith(".smat.lz4")).filter(_.startsWith("data")).sortWith(_<_);
// From list, get the number xx from dataxx.imat in listLabels, sorted (dropRight, then drop)
val nrs = listLabels.map(_.drop(4).split('.')(0)).sortBy(_.toInt)
val nrfiles = (nrs(nrs.length-1)).toInt;
//val nn:List[(Int) => String] = (nrs2 => list3)

// Files for training
val nrTrainFiles = IMat(floor(percTrain*nrfiles))(0);

// Load masterDict
val masterDict = loadDict(dicdir+"masterDict.sbmat", dicdir+"masterDict.dmat");
val nrWords = masterDict.cstr.nrows;


// Define FileDataSource
class xopts extends Learner.Options with FilesDS.Opts with GLM.Opts with ADAGrad.Opts;
val mnopts = new xopts

mnopts.fnames = List(FilesDS.simpleEnum(indir+"data%03d.smat.lz4", 1, 0), 
	      		FilesDS.simpleEnum(indir+"labels%03d.smat.lz4", 1, 0));
mnopts.nstart = 0;                 // Starting file number
mnopts.nend = nrTrainFiles;                  // Ending file number
mnopts.order = 1;                  // (0) sample order, 0=linear, 1=random
mnopts.lookahead = 2;              // (2) number of prefetch threads
mnopts.featType = 1;               // (1) feature type, 0=binary, 1=linear
mnopts.addConstFeat = true;         // add a constant feature (effectively adds a $\beta_0$ term to $X\beta$)
mnopts.sampleFiles = 1f;
mnopts.links = GLM.logistic*iones(nrValidMoods,1)
mnopts.autoReset = false
val ds = {
  implicit val ec = threadPool(4)   // make sure there are enough threads (more than the lookahead count)
  new FilesDS(mnopts)              // the datasource
}
val nn = new Learner(ds, new GLM(mnopts), null, new ADAGrad(mnopts), mnopts);
mnopts.batchSize = 10000;
mnopts.npasses = 2;
mnopts.lrate = 1; // default 1
mnopts.texp = 0.5; // default 0.5
//opts.links = GLM.maxp or 1*iones(nrMoods,1);


// Train and Predict
nn.train
var weights = FMat(nn.modelmat)


// Count MoodIds in Train Set
ds.reset
val moodCountTrain = zeros(nrValidMoods, 1);
//Did once for 90%
//var iter = 0
//while (ds.hasNext) {
//      val mats = ds.next
//      moodCountTrain ~ moodCountTrain + sum(mats(1),2)
//      println(f"Iteration ${ds.permfn(ds.fileno)}")
//      iter +=1
//}


// Test
// for now testing is in a single file
// access order is random
mnopts.nstart = nrTrainFiles+1;
mnopts.nend = nrfiles;
ds.reset

// not correct right now, but to get a bigger test set
mnopts.batchSize = 100000
ds.reset
ds.next  // this fails
ds.next  // this fails but third one works...?


//var iter = 0;
//val acc = dzeros(nrValidMoods, nrValidMoods);
//while (ds.hasNext) {
//      val mats = ds.next
//      val pred = zeros(mats(1).nrows, mats(1).ncols)
//      val (tnn, topts) = GLM.predictor(nn.model, mats(0), mats(1));
//	topts.autoReset = false;
//      tnn.predict
//      iter += 1
//}

val mats = ds.next;
//val tcatsHat = zeros(mats(1).nrows, mats(1).ncols);
val tcats = FMat(mats(1));
val moodCountTest = sum(tcats,2);
//val tcatsHat = FMat(1/(1+exp(- weights * mats(0))));
val tcatsHat = zeros(mats(1).nrows, mats(1).ncols);
val (tnn, topts) = GLM.predictor(nn.model, SMat(mats(0)), tcatsHat);
topts.autoReset = false // do not forget this
tnn.predict


// Output Results
val loglikelihood = mean(ln(1e-7f + tcats*@tcatsHat + (1-tcats)*@(1-tcatsHat)),2);
val meanll = mean(loglikelihood);
//val targacc = mean(acc, 2);
// Calculate ROC/AUC
val rr = roc2(tcatsHat, full(tcats), 1-full(tcats), 100);
val aucvec = mean(rr);
val (bestv, besti) = maxi2(aucvec);
val (worstv, worsti) = mini2(aucvec);
val overall = aucvec * sum(tcats,2)/sum(sum(tcats));


// Covariance and Correlation Matrices
   // weights of each classifier
val weightCovarMat = covarMat(weights);
val weightCorrMat = corrMat(weights);
    // predicted labels
val predCovarMat = covarMat(tcatsHat);
val predCorrMat = corrMat(tcatsHat);
    // predicted vs true labels covariance and correlation
val ptCovarMat = covarMat(tcatsHat, tcats);
val ptCorrMat = corrMat(tcatsHat, tcats);


// Confusion Matrix
   // based on one vs all
val truel = maxi2(tcats)._2.t;
val predl = maxi2(tcatsHat)._2.t;
val confMat = FMat(accum(truel\predl, 1, nrValidMoods, nrValidMoods));
confMat ~ confMat/sum(confMat,2)
val acc = mean(getdiag(confMat))
val weightedacc = getdiag(confMat).t*moodCountTest/sum(moodCountTest);

// Save Matrices
val outdir = "/home/anasrferreira/plotdata/";
saveAs(outdir+"results.mat", aucvec, "aucvec", overall, "overall", weights, "weights",
	 weightCovarMat, "weightCovarMat", weightCorrMat, "weightCorrMat",
	 predCovarMat, "predCovarMat", predCorrMat, "predCorrMat",
	 ptCovarMat, "predtrueCovarMat", ptCorrMat, "predtrueCorrMat",
	 confMat, "confMat", loglikelihood, "loglikelihood", meanll, "meanll",
	 acc, "acc", weightedacc, "weightedacc",
	 moodCountTrain, "moodCountTrain", moodCountTest, "moodCountTest",
	 FMat(percTrain), "percTrain", IMat(mnopts.batchSize), "batchSize",
	 mnopts.lrate, "lrate", IMat(mnopts.npasses), "npasses", mnopts.texp, "texp",
	 mnopts.vexp, "vexp")



//val correct = sum(maxi2(tcatsHat)._2 == labels(1,nrTrainPts->nrDataPts))
//val total = nrDataPts-nrTrainPts

//resetGPU; 
//Mat.clearCaches;
