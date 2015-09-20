// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala get_user_dict.scala

import utils._
import java.io._
import scala.io.Source._
import scala.collection.mutable.ListBuffer

val it = fromFile("/home/pierre/combined/files_trimmed.txt").getLines

var arr = new ListBuffer[String]();

while(it.hasNext) {
  var username = it.next;

  // remove .xml at the end
  username = username.substring(0, username.length()-4)

  arr.append(username);
}


var userDict = Dict(csrow(arr.toList));

// var labels = loadIMat("/var/local/destress/featurized_sent/data1.imat")
// var sents = loadSMat("/var/local/destress/featurized_sent/data1_sent.smat.lz4")

// var ix = 0;
// var curr = IMat(FMat(sents(find(sents(?, ix) != 0), ix)));
// var dict = loadDict("/var/local/destress/tokenized2/masterDict.sbmat");

// var sent = dict(curr).t;


saveSBMat("/home/pierre/combined/userDict.sbmat",SBMat(userDict.cstr));

