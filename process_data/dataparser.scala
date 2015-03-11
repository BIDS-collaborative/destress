// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala


import utils._  // utility methods

val dir = "/var/local/destress/tokenized/";
val xmlFile = loadIMat(dir+"zz.xml.imat");
val newdict = loadDict(dir+"zz_dict.sbmat",dir+"zz_dict.imat")


val usersIdx = getBeginEnd(xmlFile, newdict, "posts");
val postIdx = getBeginEnd(xmlFile, newdict, "post");
val eventIdx = getBeginEnd(xmlFile, newdict, "event");
val moodIdx = getBeginEnd(xmlFile, newdict, "current_moodid");


// Get nrUsers
var nrUsers = 0
if (usersIdx(0,0) != -1) {nrUsers = size(usersIdx,1)}


// Get nrValidPosts
// Posts where <event><string>...</string></event>
// If </string> is not parsed properly, discard
val validEvent = find(xmlFile(eventIdx(?,1)-1) == newdict("</string>"));
// If don't want to discard just append
// find(xmlFile(eventIdx(?,1)-1) == newdict("string"));

val nrValidPosts = size(validEvent, 1);
// valEventIdx points to <string> +1 and to </string>: do col(0)->col(1)
var valEventIdx = eventIdx(validEvent, ?) + (1\ -1);

val valpostIdx = postIdx(validEvent,?); //assumes postIdx.nrows == eventIdx.nrows


// Get nrWords in dictionary
val nrWords = newdict.cstr.nrows;


// Initialize structures
// For sparse posts
var ii=List[Int]();
var jj=List[Int]();
var vv=List[Int]();

// For now, IMat with (UserId, CurrentMoodId) - later do datetime+replycount
var labels = izeros(nrValidPosts, 2);

var posti = 0; // iteration counter for while
var userk = 0; // current userid

var moodIdx = irow(0,0)
var moodid = -1; // current moodid

var postStart = 0
var postEnd = 0

while (posti < nrValidPosts) {

      postStart = valpostIdx(posti, 0)
      postEnd   = valpostIdx(posti, 1)

      // Increment userk until the first index of the post is less
      // than the last index of the current user
      while (postStart > usersIdx(userk, 1)) userk += 1

      // Get indices of "current_moodid" open and close tags 
      moodIdx = getBeginEnd(xmlFile(postStart -> postEnd), newdict, "current_moodid");
      // Check to make sure the tags appear and that it is an int
      if (moodIdx.nrows==1 && newdict(xmlFile(postStart+moodIdx(0,0))) =="<int>") {
           moodid = twoComplementToInt(xmlFile(postStart+moodIdx(0,0)+1))(0)  
	   }

      	 // Add to "labels" -> userid, currentmoodId


	 // Add to cols/rows/values for future sparse matrix
     	 
//	  var postWordId = getWordsOnly(xmlFile, valEventIdx(posti,0), valEventIdx(posti,1));

      posti += 1;
}


// For each new point, do jj = jj.+:(1);

// At the end
var newsparse = sparse(icol(ii), icol(jj), col(vv), nrValidPosts, nrWords);









//COMMENTS ONLY

//Get not a number values
//val ind = xmlFile(moodIdx(1,0)+find(xmlFile(moodIdx(1,0)->moodIdx(1,1))>0))
//newdict(ind)
//Get number values -> need to be converted
//val neg = xmlFile(moodIdx(1,0)+find(xmlFile(moodIdx(1,0)->moodIdx(1,1))<0))
//twoComplementToInt(neg)

////////////////////////////////
// TODO -> Create Features
// and Labels
////////////////////////////////

// IMat with 2 cols: useridNr | MoodIdNr
// moodId -> integer 1->134 (no 94 nor 50)
// useridNr -> for now it's just a counter (i.e., new "<posts>...</posts>" means a new user)


// Now, create sparse object for
// posts, userid, moodid

// var userIdCounter = 0;
// val incSize = 200; //increase size of moodId when needed
// var moodId = izeros(200,2);
//sum(newdict.counts)
//maxi(newdict.counts)
//Create sparse matrix with Posti (row) Wordsj (cols) and nrCountsPostiWordj (vals)

// there is no way to update a particular sparse matrix unless that index exists
// if it doesn't we need to reconstruct it again
// seems like the best is to create List of ii's, jj's, and vv's (using prepend .+:)
// after that convert



full(newsparse)
newsparse(?,0\1\2)

// HERE