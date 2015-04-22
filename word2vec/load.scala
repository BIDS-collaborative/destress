// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala load.scala

import utils._  // utility methods
import java.io._
import scala.io.Source
import scala.math

import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

var dictdir = "/var/local/destress/tokenized/"; 
var trainingdir = "/var/local/destress/"; 

val masterDict = loadDict(dictdir+"masterDict.sbmat",dictdir+"masterDict.dmat"); 
val trainedMatrix = loadFMat(trainingdir+"vectors.txt"); 

val source = Source.fromFile("/home/yxing/destress/word2vec/oneLiner.txt");
val lines = try source.mkString finally source.close();
val words = lines.split(" ");
val listwords = words.toList; 
val a = csrow(listwords)
val index = irow(List.range(1,3000001)) 
val googleDict = Dict(a, index)

val pw = new PrintWriter(new File("MATRIX.txt"))



//val cs = CSMat(3000000, 1,  
//val zeros = irow(List.fill(300)(0))

for ( i <- 1 until masterDict.length) {
	val lookup = masterDict(i); 
	if (googleDict(lookup) != -1 ) {
		pw.write( trainedMatrix(googleDict(lookup), ?) + "\n"); 
	}
	else {
		pw.write( List.fill(300)(0) mkString (",") );
		pw.write("\n");  
	
	}
}

pw.close;




