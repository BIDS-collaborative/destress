import utils._;

// Combine the dictionaries into masterDict
// combine_dicts(xmlList:String,directory:String,maxDictItems:Int = 1000000):
var masterDict=combine_dicts("/var/local/destress/tokenized/fileList.txt","/var/local/destress/tokenized/");

// Reorder dictionary in descending frequency
var sortIdx:IMat = sortdown2(masterDict.counts)._2
masterDict=Dict(masterDict.cstr(sortIdx),masterDict.counts(sortIdx))

// Save dictionaries
saveSBMat("/var/local/destress/tokenized/masterDict.sbmat",SBMat(masterDict.cstr));
saveDMat("/var/local/destress/tokenized/masterDict.dmat",masterDict.counts);

println("FINISHED");