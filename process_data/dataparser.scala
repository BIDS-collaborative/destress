// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._  // utility methods

val dir = "/var/local/destress/tokenized/";
val fileListPath= dir+"fileList.txt";

val masterDict = loadDict(dir+"masterDict.sbmat",dir+"masterDict.dmat");

// Get nrWords in master dictionary
val nrWords = masterDict.cstr.nrows;

//Get list of xml files from input file. 
var fileList = Source.fromFile(fileListPath).getLines().toList;

// Save various stats in these variables
var nrUsers = 0; // Total number of users
var nrValidPosts = 0; // Total number of posts with <event><string>...</string></event> form

// Initialize structures
var sBoWposts = sparse(izeros(nrWords,0)); // Sparse matrix of feature vectors
var labels = izeros(2,0); // Dense IMat with (UserId, CurrentMoodId) - later do datetime+replycount

//Go through list:
for (line <- fileList.drop(1)) {

	// Get the current xml file data and original dictionary
	val xmlFile = loadIMat(dir+line+".xml.imat");
	val xmlDict = loadDict(dir+line+"_dict.sbmat",dir+line+"dict.imat");

  // Map from the native dictionary to merged dictionary
	val mapToMaster = xmlDict-->masterDict;

	val usersIdx = getBeginEnd(xmlFile, xmlDict, "posts"); // Indexes for <posts> and </posts> (which enclose all activity by one user)
	val postIdx = getBeginEnd(xmlFile, xmlDict, "post"); // Indexes for <post> and </post> (which enclose each activity by a user)
	val eventIdx = getBeginEnd(xmlFile, xmlDict, "event"); // Indexes for <event> and </event> (which could be a text post)
	val moodIdx = getBeginEnd(xmlFile, xmlDict, "current_moodid"); // Indexes for moodid tags

	// Get nrValidPosts
	// Valid posts are of the form <event><string>...</string></event>
	// If </string> is not parsed properly, the post is discarded
	val validEvent = find(xmlFile(eventIdx(?,1)-1) == xmlDict("</string>"));
	// If don't want to discard just append
	// find(xmlFile(eventIdx(?,1)-1) == xmlDict("string"));

  // Update total number of valid posts
	nrValidPosts += validEvent.nrows;
  
	// valEventIdx points to <string> +1 and to </string>: do col(0)->col(1)
	val valEventIdx = eventIdx(validEvent, ?) + (1\ -1);
	val valpostIdx = postIdx(validEvent,?); //assumes postIdx.nrows == eventIdx.nrows

	var posti = 0; // iteration counter for while

	while (posti < validEvent.nrows) {

		val postStart = valpostIdx(posti, 0);
		val postEnd   = valpostIdx(posti, 1);
    
    var userk: Int = 0;

		// Increment userk until the first index of the post is less
		// than the last index of the current user
		while (postStart > usersIdx(userk, 1)) userk += 1;

		// Get indices of "current_moodid" open and close tags 
		val moodIdx = getBeginEnd(xmlFile(postStart -> postEnd), xmlDict, "current_moodid");

		// Check to make sure the current_moodid exists and that it is an int
		if (moodIdx.nrows==1 && xmlDict(xmlFile(postStart+moodIdx(0,0))) =="<int>") {

			val moodid = twoComplementToInt(xmlFile(postStart+moodIdx(0,0)+1))(0);

			// Add to "labels" -> userid, currentmoodId
			labels \= icol(userk+nrUsers, moodid);

			// Get the post text, discarding numbers     	 
			var postWordId = getWordsOnly(xmlFile, valEventIdx(posti,0), valEventIdx(posti,1));
			// Map the text to the masterDict
			postWordId = mapToMaster(postWordId)
					// Discard -1's corresponding to words which aren't in the masterDict
					postWordId = postWordId(find(postWordId>=0))
					// Create a sparse column with the BoW from this post
					val temp = sparse(postWordId,izeros(postWordId.nrows,1),iones(postWordId.nrows,1), nrWords,1);
			// Add sparse column of current post BoW, to full BoW by horizontal concatenation
			sBoWposts \= temp;
		}

		// Next Post
		posti += 1;
	}
  
	// Update nrUsers to include this file
	if (usersIdx(0,0) != -1) {nrUsers += usersIdx.nrows}

}



//COMMENTS ONLY

//Get not a number values
//val ind = xmlFile(moodIdx(1,0)+find(xmlFile(moodIdx(1,0)->moodIdx(1,1))>0))
//xmlDict(ind)
//Get number values -> need to be converted
//val neg = xmlFile(moodIdx(1,0)+find(xmlFile(moodIdx(1,0)->moodIdx(1,1))<0))
//twoComplementToInt(neg)


// moodId -> integer 1->134 (no 94 nor 50)
//sum(xmlDict.counts)
//maxi(xmlDict.counts)
