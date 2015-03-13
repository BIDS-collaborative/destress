// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._  // utility methods

val dir = "/var/local/destress/tokenized/";
val xmlFile = loadIMat(dir+"zz.xml.imat");
val xmlDict = loadDict(dir+"zz_dict.sbmat",dir+"zz_dict.imat");

val masterDict = loadDict(dir+"mastrDict.sbmat",dir+"masterDict.imat");
val mapToMaster = xmlDict-->masterDict 

val usersIdx = getBeginEnd(xmlFile, xmlDict, "posts");
val postIdx = getBeginEnd(xmlFile, xmlDict, "post");
val eventIdx = getBeginEnd(xmlFile, xmlDict, "event");
val moodIdx = getBeginEnd(xmlFile, xmlDict, "current_moodid");


// Get nrUsers
var nrUsers = 0;
if (usersIdx(0,0) != -1) {nrUsers = usersIdx.nrows}


// Get nrValidPosts
// Posts where <event><string>...</string></event>
// If </string> is not parsed properly, discard
val validEvent = find(xmlFile(eventIdx(?,1)-1) == xmlDict("</string>"));
// If don't want to discard just append
// find(xmlFile(eventIdx(?,1)-1) == xmlDict("string"));

val nrValidPosts = validEvent.nrows;
// valEventIdx points to <string> +1 and to </string>: do col(0)->col(1)
var valEventIdx = eventIdx(validEvent, ?) + (1\ -1);
val valpostIdx = postIdx(validEvent,?); //assumes postIdx.nrows == eventIdx.nrows


// Get nrWords in dictionary
val nrWords = xmlDict.cstr.nrows;


// Initialize structures
// For sparse posts
var sBoWposts = sparse(izeros(nrWords,0));
// For now, IMat with (UserId, CurrentMoodId) - later do datetime+replycount
var labels = izeros(2,0);

var posti = 0; // iteration counter for while
var userk = 0; // current userid


while (posti < nrValidPosts) {

      val postStart = valpostIdx(posti, 0)
      val postEnd   = valpostIdx(posti, 1)

      // Increment userk until the first index of the post is less
      // than the last index of the current user
      while (postStart > usersIdx(userk, 1)) userk += 1

      // Get indices of "current_moodid" open and close tags 
      val moodIdx = getBeginEnd(xmlFile(postStart -> postEnd), xmlDict, "current_moodid");
      // Check to make sure the tags appear and that it is an int
      if (moodIdx.nrows==1 && xmlDict(xmlFile(postStart+moodIdx(0,0))) =="<int>") {
      	  val moodid = twoComplementToInt(xmlFile(postStart+moodIdx(0,0)+1))(0);

	  // Add to "labels" -> userid, currentmoodId
	  labels \= icol(userk, moodid);

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
