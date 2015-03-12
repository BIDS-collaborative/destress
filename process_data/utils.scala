// Utility Functions to Process Data after xmltweet
import scala.io.Source
object utils {

    // Returns pairs of indices in a xml.imat file with the beginning and end of a specific xml tag
    // Input: xmlImat:BIDMat.IMat - imat (from a xml.imat file)
    // 	  dict:BIDMat.Dict - dictionary (from dictionary words file)
    //	  xmlTagName:String - eg "posts", "events", "current_moodid"
    // Output: (begin, end): (IMat,IMat)
    //   	   begin - vector with indices for "<xmlTagName>" [+1] (if shiftBegInd)   
    //		   end - vector with indices for "</xmlTagName>"   

    def getBeginEndIndependent(xmlImat:BIDMat.IMat, dict:BIDMat.Dict, xmlTagName:String, shiftBegInd:Boolean = true): (BIDMat.IMat, BIDMat.IMat) = {
    	// assume dict(0) is just a dummy string
	var begin = find(xmlImat == dict("<"+xmlTagName+">")); //index of taglocation
	if (shiftBegInd) { begin += 1; } //index of taglocation+1
	val end = find(xmlImat == dict("</"+xmlTagName+">"))
	(begin, end)
    }

    def getBeginEnd(xmlImat:BIDMat.IMat, dict:BIDMat.Dict, xmlTagName:String, shiftBegInd:Boolean = true): BIDMat.IMat = {
    	// this assumes dict(0) is just a dummy string
	val (begin, end) = getBeginEndIndependent(xmlImat, dict, xmlTagName, shiftBegInd);
	if (begin.size() != end.size()) {
	   irow(-1) \ irow(-1)
	}
	else {
    	   begin \ end
	}
	// note about the "+1" for begin
	// col 0 has index after <xmltag> and col1 has index of </xmltag>
	// allows indexing with elcol0->elcol1 and get the content inside only
    }

    // OLD def twoComplementToInt(twoComp:BIDMat.IMat): BIDMat.IMat = {Int.MinValue+twoComp}

    def twoComplementToInt(twoComp:BIDMat.IMat): BIDMat.IMat = {
    	// Fixes problem with originally negative numbers having identity map
	// Original -12 -> xmlFile -12 and Original 2 -> -2147483646=Int.MinVal+2
    	var out = twoComp(?);  //doesn't change originally negative nrs (2^30->0)
	out(find(twoComp<Int.MinValue/2)) += Int.MinValue
	out
    }

    def loadDict(sbmatFile:String, imatFile:String): BIDMat.Dict = {
    	val dfiller = Dict(csrow("<xx>"), irow(0)); //dummy to use up index 0
	val dWords = CSMat(loadSBMat(sbmatFile)); // load dict.sbmat
	val dCount = loadIMat(imatFile); // load dict.imat
	val dict = Dict(dWords, dCount); // combine to dictionary

	Dict.union(dfiller,dict) // union dictionary with dummy
    }

    def getWordsOnly(xmlImat:BIDMat.IMat, bIdx:Int, eIdx:Int): BIDMat.IMat = {
    	val temp = xmlImat(bIdx->eIdx);
	temp(find(temp>0)) //these are dictionary indices
    }
    
        /*Method to combine multiple dictionaries into a single BIDMat.Dict.
   //Inputs:
   -directory: directory containing xmlfile imat and dict [sbmat,imat] that were output from xmltweet.
   -xmlList: input file containing a list of the names of the xml files.
   -maxDictItems: Maximum number of dictionary items before trimming
  //Returns:
  -BIDMat.Dict object containing a combination of all the counts of the dictionaries.
  //Example call:
  //>val k:BIDMat.Dict=combine_dicts("allXmlNames.txt","/Users/helgammal/Downloads/BIDMach_1.0.0-osx-x86_64/src/main/C/newparse")
  //>k.counts("<posts>")
*/
def combine_dicts(xmlList:String,directory:String,maxDictItems:Int = 1000000): BIDMat.Dict = {
	
	//Get list of xml files from input file. 
	var fileList = Source.fromFile(xmlList).getLines().toList;
	
	//Initialize currentDict.
	var someIMat:BIDMat.IMat = loadIMat(directory+"/"+fileList(0)+".xml.imat");
    var currentDict:BIDMat.Dict = loadDict(directory+"/"+fileList(0)+"_dict.sbmat",directory+"/"+fileList(0)+"_dict.imat");
   
	var finalDict: BIDMat.Dict = currentDict;
	
	// print progress
	println(s"Processing tokenized files from ${fileList(0)}.xml")

	// Keep track of the current minimum threshold before trimming
	var threshold=1

	//Go through list:
	for (line <- fileList.drop(1)) 
	{
	   // Print progress
	   println(s"Processing tokenized files from ${line}.xml")
		someIMat = loadIMat(directory+"/"+line+".xml.imat");
        currentDict = loadDict(directory+"/"+line+"_dict.sbmat",directory+"/"+line+"_dict.imat");
        finalDict = Dict.union(finalDict,currentDict);
	

	   // Automatically trim until number of dictionary entries less 
	   // than maxDictItems
	   if (finalDict.length>maxDictItems) finalDict=finalDict.trim(threshold)
	   while (finalDict.length>maxDictItems) {
	   	 threshold+=1;
		 // Trims dict entries with less than threshold counts
	         finalDict=finalDict.trim(threshold); 
	 	 // Notify user
		 println(s"\nDictionary trim threshold increased to $threshold.\n")		  }
	   // TO DO: RE-PAD THE DICTIONARY AFTER TRIM?
	}

    // Trim the dictionary to the current threshold for consistency
    finalDict.trim(threshold)
}

}
