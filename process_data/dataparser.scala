// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._  // utility methods

val indir = "/var/local/destress/tokenized/";
val outdir = "/var/local/destress/featurized/"
val fileListPath= indir+"fileList.txt";

val maxMb = 25 // Approximate size of sparse matrix save batches (uncompressed)
val postPerMb = 1000 // (Very) approximate number of posts which take 1Mb of saved space. May change!

val masterDict = loadDict(indir+"masterDict.sbmat",indir+"masterDict.dmat");

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

// Keeps track of number of batches, increasing each time one is saved
var batchNumber = 1

//Go through list:
for (line <- fileList) {

	// Print a status update so the user can see something is happening
	println(s"Currently featurizing ${line}.xml");

	// Get the current xml file data and original dictionary
	val xmlFile = loadIMat(indir+line+".xml.imat");
	val xmlDict = loadDict(indir+line+"_dict.sbmat",indir+line+"_dict.imat");

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
	val valEventIdx: IMat = if (eventIdx.nrows>0 && validEvent.nrows>0) eventIdx(validEvent, ?) + (1\ -1) else izeros(0,2);
	val valpostIdx: IMat = if (postIdx.nrows==eventIdx.nrows) postIdx(validEvent,?) else izeros(0,2); //assumes postIdx.nrows == eventIdx.nrows

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

			// Write the features to a file once a size threshold is passed
			if (labels.ncols==maxMb*postPerMb) {

				println(s"\nWriting batch $batchNumber to file.\n");

				saveSMat(outdir+"data"+s"$batchNumber"+".smat.lz4",sBoWposts); // Compress the sparse matrices, saves about half the disk space
				sBoWposts = sparse(izeros(nrWords,0)); // Reset the sparse matrix

				saveIMat(outdir+"data"+s"$batchNumber"+".imat",labels); // These are very small, no reason to compress
				labels = izeros(2,0); // Reset the labels matrix

				batchNumber+=1;

			}

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
