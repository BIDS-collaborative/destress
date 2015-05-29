// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

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

// Input directory for test and train data
val indir = "/var/local/destress/preprocessed6/"; // This one has 75000 words

val (nn,opts)= DNN.learnerX(indir+"trainData%03d.smat.lz4",indir+"trainLabels%03d.fmat.lz4");

opts.aopts = opts; // I think this sets the ADAGrad options to opts

// FilesDS opts
//opts.nstart = 0;                 // Don't set these, and BIDMach will automatically run over all files
//opts.nend = nrTrainFiles;
opts.order = 1;                  // (0) sample order, 0=linear, 1=random
opts.lookahead = 2;              // (2) number of prefetch threads
opts.featType = 1;               // (1) feature type, 0=binary, 1=linear
opts.addConstFeat = false;        // add a constant feature (effectively adds a $\beta_0$ term to $X\beta$)

// ADAGrad and training options
opts.batchSize=1000;
opts.reg1weight = 0.0001;
opts.lrate = 0.5f;
//opts.waitsteps = 10;
opts.texp = 0.4f;
//opts.vexp = 0.6f;
opts.npasses = 3;

// DNN Opts
opts.links = iones(nrValidMoods,1);

/*
 * Build a stack of layer specs. layer(0) is an input layer, layer(n-1) is a GLM layer. 
 * Intermediate layers are FC alternating with ReLU, starting and ending with FC. 
 * First FC layer width is given as an argument, then it tapers off by taper 
 * (each layer width is taper*previous width).
 * nonlin specifies activation function: 1-> tanh, 2-> sigmoid, 3-> ReLU, 4-> Softplus
 * 
 * def dlayers(depth0:Int, width:Int, taper:Float, ntargs:Int, opts:Opts, nonlin:Int = 1)
 */
DNN.dlayers(5,200,0.5f,nrValidMoods,opts,2);

// Train model
nn.train;

// Test model on subset of test set
// First creat a FilesDS of test data
opts.fnames = List(FilesDS.simpleEnum(indir+"testData%03d.smat.lz4", 1, 0), 
    FilesDS.simpleEnum(indir+"testLabels%03d.fmat.lz4", 1, 0));
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
var aucvec = dzeros(1,nrValidMoods);

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

  val tcats = FMat(mats(1)); // Ground truth labels

  val prevTestPoints:Float = if(testCount.ncols>0) sum(testCount)(0)*1.0f else 0f; // Number of test points already processed
  testCount = testCount \ icol(mats(1).ncols); 

  val tcatsHat = zeros(tcats.nrows, tcats.ncols); // Create empty matrix to fill with predicted labels
  val (tnn, topts) = DNN.predictor(nn.model, SMat(mats(0)), tcatsHat); // Create predictor from model

  topts.autoReset = false; // It seems that without this, each predict erases the weights in the trained model

  tnn.predict; // execute prediction

  // Calculate AUCs, combining with previous AUCs by averaging
  val rr = roc2(tcatsHat, full(tcats), 1-full(tcats), rocRes);
  aucvec = (aucvec*prevTestPoints+mean(rr)*(mats(1).ncols*1.0f) ) / sum(testCount) ;

}

mean(aucvec)
maxi2(aucvec)
mini2(aucvec)