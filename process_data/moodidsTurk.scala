//Reduce the number of moodIDs from 132 to 14 emotions categories + 1 non labeled category

import utils._  // utility methods

import scala.io.Source
import scala.math

import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

val indir="/var/local/destress/featurized/"

var item=1
val Labels=Array.ofDim[Int](15,50)  // Label mapping --> rows are categories
Labels(0)=Array(122,35,15,125,131,66,72,99,21,26,93,109,126,13,32,42,67,70,105,115,116,117,121)
Labels(1)=Array(62,98)
Labels(2)=Array(10,11,41,52,59,120,55,123,65,17,36,44,96)
Labels(3)=Array(101,85,69,87,89,16,45,56,73,88,128)
Labels(4)=Array(1,3,46,80,110,118,133,85)
Labels(5)=Array(4,134,63,104)
Labels(6)=Array(19,20,95,28,47,100,37)
Labels(7)=Array(2,8,12,24,23,106,119)
Labels(8)=Array(30,25,38,39,29)
Labels(9)=Array(127,7,9,22,124,129,114,107,43,60,71,79)
Labels(10)=Array(77,75,5,33,113,78,103)
Labels(11)=Array(58,57,68,92,102)
Labels(12)=Array(53,61)
Labels(13)=Array(76,64,86)
Labels(14)=Array(112,91,54,90,14,31,40,49,51,132,74,108,6,18,27,34,48,81,82,83,84,97,111,130)

while(item>0){
	val moodid=loadIMat(indir+"data"+f"$item"+".imat")
	for (iter<- 0 to length(moodid)/2-1){
	  for(emos<- 0 to Labels.size-1){
	    if(Labels(emos) contains moodid(1,iter)){
        moodid(1,iter)=emos
      }
    }
	}
  saveIMat(indir+"data"+f"$item"+"s.imat",moodid)
  println("saving file "+moodid+"s.scala")
  item += 1
}
