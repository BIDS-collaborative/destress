// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._  // utility methods
import StringBuilder.scala
import scala.io.Source
import scala.math

import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._


Object ScannerTest {
  def main(args: Array[String]) {
	var b= new StringBuilder(100000000000000000000000000000)
    for (ln <- Source.fromPath("/var/local/destress/words.txt").getLines){
		b.append(ln); 
		b.append(" "); 
	}
  }
  return sb.toString(); 

}
