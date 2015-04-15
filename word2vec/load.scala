// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala load.scala

import utils._  // utility methods

import scala.io.Source
import scala.math

import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

var dictdir = "/var/local/destress/tokenized/"; 
var trainingdir = "/var/local/destress/"; 

//val masterDict = loadDict(dictdir+"masterDict.sbmat",dictdir+"masterDict.dmat"); 
val training = loadFMat(trainingdir+"vectors.txt"); 

val 




