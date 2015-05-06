// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import BIDMach.models._
import utils._   // utility methods
import java.io.File
import java.nio.file.{Paths, Files}
import scala.util.Random

resetGPU;
Mat.clearCaches;

// Constants
val nrMoods = 132+1+2;
val nrValidMoods = nrMoods-3;
val validMoodIdx = (1->50) \ (51->94) \ (95->nrMoods) // 0,50,94 aren't valid
val batchSize = 10000;

// Get listofLabels and BagOfWords - sort 
val indir = "/var/local/destress/preprocessed5/";

// Build parameters to CV on
val lrates = List(.01f,.1f,1.0f);
val texps = List(.25f,.5f);
val regweights = List(1.0e-8,1.0e-7f,1.0e-6f);

var lrateparams = zeros(0,1);  // ( (lrates kron ones(texps.nrows,1)) kron ones(toUse,1);
var texpparams = zeros(0,1); // (ones(lrates.nrows,1) kron texps) kron ones(toUse,1);
var regparams= zeros(0,1);

for (lrate <- lrates) {
  for (texp <- texps) {
    for (regweight <- regweights) {
      lrateparams = lrateparams on col(lrate);
      texpparams = texpparams on col(texp);
      regparams = regparams on col(regweight);
    }
  }
}

val nParams = lrateparams.nrows; // Number of points in grid search

val toUse=25;
var targmaps = (mkdiag(ones(toUse,1)) \ zeros(toUse,nrValidMoods-toUse));
val nLabels=targmaps.nrows; // Number of labels trained for each set of params

// Define FileDataSource
class xopts extends Learner.Options with FilesDS.Opts with GLM.Opts with ADAGrad.Opts with L1Regularizer.Opts;
val opts = new xopts;

opts.fnames = List(FilesDS.simpleEnum(indir+"trainData%03d.smat.lz4", 1, 0), 
                     FilesDS.simpleEnum(indir+"trainLabels%03d.smat.lz4", 1, 0));
//opts.nstart = 0;                 // Don't set these, and BIDMach will automatically run over all files
//opts.nend = 10;
opts.order = 1;                  // (0) sample order, 0=linear, 1=random
opts.lookahead = 2;              // (2) number of prefetch threads
opts.featType = 1;               // (1) feature type, 0=binary, 1=linear
opts.addConstFeat = true;        // add a constant feature (effectively adds a $\beta_0$ term to $X\beta$)

opts.links = GLM.logistic*iones(nLabels*nParams,1)  // What does this do?

val trainDS = {
  implicit val ec = threadPool(4);   // make sure there are enough threads (more than the lookahead count)
  new FilesDS(opts)              // the datasource
}

// Create learner
val nn = new Learner(trainDS, 
                     new GLM(opts), 
                     Array(new L1Regularizer(opts)), 
                     new ADAGrad(opts), 
                     opts);

// Set learner options
opts.batchSize = batchSize;
opts.npasses = 3;

opts.targmap = ones(nParams,1) kron targmaps;
opts.lrate = lrateparams kron ones(nLabels,1);
opts.texp = texpparams kron ones(nLabels,1);
opts.reg1weight = regparams kron ones(nLabels,1);  

// Train model
nn.train
var weights = FMat(nn.modelmat) // Save weights

// Test model on subset of test set
// First creat a FilesDS of test data
opts.fnames = List(FilesDS.simpleEnum(indir+"testData%03d.smat.lz4", 1, 0), 
    FilesDS.simpleEnum(indir+"testLabels%03d.smat.lz4", 1, 0));
opts.batchSize = 100000;  // Read full file each time
opts.nstart = 0;                 
opts.nend = 7;            // Only use first 7 (out of 15) files, save the rest for a real test later
opts.featType = 1;        // (1) feature type, 0=binary, 1=linear

val testDS = {
    implicit val ec = threadPool(4);   // make sure there are enough threads (more than the lookahead count)
    new FilesDS(opts);              // the datasource
}

testDS.init; 

// Calculate AUCs over the selected test files
var testCount=izeros(1,0) // Each column entry records the number of data points in a particular test file
val rocRes = 100;
var aucvec = dzeros(1,nParams*nLabels);

var mats: Array[BIDMat.Mat]=null;

// First calculate ROCs
while(testDS.hasNext) {
  
  resetGPU; 
  
  // Bag-of-words features. Sometimes the first time next is called an oob error is thrown, but
  // I really don't know why. Just calling next again seems to work
  try{
    mats = testDS.next
  }
  catch{
    case oob: java.lang.ArrayIndexOutOfBoundsException => mats = testDS.next
  }

  val tcats =  (ones(nParams,1) kron targmaps) * FMat(mats(1)); // Ground truth labels

  val prevTestPoints:Float = if(testCount.ncols>0) sum(testCount)(0)*1.0f else 0f; // Number of test points already processed
  testCount = testCount \ icol(mats(1).ncols); 

  val tcatsHat = zeros(tcats.nrows, tcats.ncols); // Create empty matrix to fill with predicted labels
  val (tnn, topts) = GLM.predictor(nn.model, SMat(mats(0)), tcatsHat); // Create predictor from model

  topts.autoReset = false; // It seems that without this, each predict erases the weights in the trained model

  tnn.predict; // execute prediction

  // Calculate AUCs, combining with previous AUCs by averaging
  val rr = roc2(tcatsHat, full(tcats), 1-full(tcats), rocRes);
  aucvec = (aucvec*prevTestPoints+mean(rr)*(mats(1).ncols*1.0f) ) / sum(testCount) ;

}

// Calculate weighted AUCs from ROCs
var aucvec2 = dzeros(nParams,1);
for ( i <- 0 until nParams )  {
//  aucvec2(i)=mean(aucvec(i*nrValidMoods until (i+1)*nrValidMoods));
  aucvec2(i)=mean(aucvec(i*nLabels until (i+1)*toUse));

}

// Find best parameters
val (bestv, besti) = maxi2(aucvec2);

val bestlrate = lrateparams(besti);
val besttexp = texpparams(besti);
val regWeight = regparams(besti);

/*
var testCount=izeros(1,0) // Each column entry records the number of data points in a particular test file

var moodCountTest = zeros(nrValidMoods,1) // Saves the distribution of moodIDs in the test files

val rocRes = 100;
var rr = dzeros(rocRes+1,nrValidMoods);

var confMat = zeros(nrValidMoods,nrValidMoods);

// Initialize here covariance/correlation matrices and calculate later 
var weightCovarMat: FMat=null;
var weightCorrMat: FMat=null;
var predCovarMat: FMat=null;
var predCorrMat: FMat=null;
var ptCovarMat: FMat=null;
var ptCorrMat: FMat=null;

while(testDS.hasNext) {
  val mats = testDS.next; // Bag-of-words features
  val tcats = FMat(mats(1)); // Ground truth labels
  
  moodCountTest = moodCountTest + sum(tcats,2); // Record distribution of moodIDs in this test file
  
  val prevTestPoints:Float = if(testCount.ncols>0) sum(testCount)(0)*1.0f else 0f; // Number of test points already processed
  testCount = testCount \ mats(1).ncols; 
  
  val tcatsHat = zeros(mats(1).nrows, mats(1).ncols); // Create empty matrix to fill with predicted labels
  val (tnn, topts) = GLM.predictor(nn.model, SMat(mats(0)), tcatsHat); // Create predictor from model

  topts.autoReset = false; // It seems that without this, each predict erases the weights in the trained model
  
  tnn.predict; // execute prediction
  
  // Calculate ROC, combining with previous ROC by averaging
  rr = (prevTestPoints*rr+roc2(tcatsHat, full(tcats), 1-full(tcats), rocRes)*(mats(1).ncols*1.0f) ) / sum(testCount) ;

  // Confusion Matrix
  // based on one vs all
  val truel = maxi2(tcats)._2.t;
  val predl = maxi2(tcatsHat)._2.t;
  confMat ~ confMat+FMat(accum(truel\predl, 1, nrValidMoods, nrValidMoods));
  
  // Covariance and Correlation Matrices calculated on last test set only
  if(!testDS.hasNext) {
    // weights of each classifier
    weightCovarMat = covarMat(weights);
    weightCorrMat = corrMat(weights);
    // predicted labels (on last test set only)
    predCovarMat = covarMat(tcatsHat);
    predCorrMat = corrMat(tcatsHat);
    // predicted vs true labels covariance and correlation (on last test set only)
    ptCovarMat = covarMat(tcatsHat, tcats);
    ptCorrMat = corrMat(tcatsHat, tcats);
  }
  
}

// Normalize the confusion matrix
confMat ~ confMat/sum(confMat,2);
val acc = mean(getdiag(confMat)); // Accuracy of classifier for each category
val weightedacc = getdiag(confMat).t*moodCountTest/sum(moodCountTest); // Weighted average accuracy

// Calculate AUC, the best and worst values, and a weighted average over categories
val aucvec = mean(rr);
val (bestv, besti) = maxi2(aucvec);
val (worstv, worsti) = mini2(aucvec);
val overall = aucvec * moodCountTest/sum(moodCountTest);

*/

//resetGPU; 
//Mat.clearCaches; 
