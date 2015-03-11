// Utility Functions to Process Data after xmltweet




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

}
