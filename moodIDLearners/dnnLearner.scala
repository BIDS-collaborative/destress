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
val indir = "/var/local/destress/preprocessed5/"; // This one has only 25000 words

//val (nn,opts)= DNN.learnerX(indir+"trainData%03d.smat.lz4",indir+"trainLabels%03d.smat.lz4");
val (nn,opts)= DNN.learnerX(loadSMat(indir+"trainData000.smat.lz4"),loadSMat(indir+"trainLabels000.smat.lz4"));

opts.aopts = opts // I think this sets the ADAGrad options to opts

// FilesDS opts
//opts.nstart = 0;                 // Don't set these, and BIDMach will automatically run over all files
//opts.nend = nrTrainFiles;
//opts.order = 1;                  // (0) sample order, 0=linear, 1=random
//opts.lookahead = 2;              // (2) number of prefetch threads
opts.featType = 1;               // (1) feature type, 0=binary, 1=linear
opts.addConstFeat = false;        // add a constant feature (effectively adds a $\beta_0$ term to $X\beta$)

// ADAGrad and training options
opts.batchSize=500;
opts.reg1weight = 0.0001;
opts.lrate = 0.2f;
opts.texp = 0.4f;
opts.npasses = 5;

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

DNN.dlayers(3,100,0.25f,nrValidMoods,opts,2);

// Train model
nn.train;