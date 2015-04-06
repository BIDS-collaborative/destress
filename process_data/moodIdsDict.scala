// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import scala.io.Source

val indir = "/home/anasrferreira/destress/moodids_dict.csv";

val src = Source.fromFile(indir).getLines()
val headerLine = src.take(1).next
// val header = headerLine.split(",")

var mainList: List[Array[String]] = List();

for(l <- src) {
      val m = 
      mainList ::= l.split(",")
}

//Add inexistent values
if (mainList.filter(x => x(0)=="0").size == 0) {
   mainList ::= Array("0", "empty");
}
if (mainList.filter(x => x(0)=="50").size == 0) {
   mainList ::= Array("50", "invalid");
}
if (mainList.filter(x => x(0)=="94").size == 0) {
   mainList ::= Array("94", "invalid2");
}

// Sort by value
mainList = mainList.sortBy(_(0).toInt)

val dict = Dict(csrow(mainList.map(x => x(1))), irow(mainList.map(x => x(0).toInt)));
saveSBMat("/var/local/destress/moodDict.sbmat", SBMat(dict.cstr))