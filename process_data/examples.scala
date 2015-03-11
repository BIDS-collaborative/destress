// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala
// this can be fixed

import utils._  // utility methods

val dir = "/var/local/destress/tokenized/";
// val dictCount = loadIMat(dir+"zz_dict.imat");
// val dictWords = Dict(loadSBMat(dir+"zz_dict.sbmat"));
val xmlFile = loadIMat(dir+"zz.xml.imat");

// newdict with a Dummy value at index 0
// loadDict is a method in utils.scala
val newdict = loadDict(dir+"zz_dict.sbmat",dir+"zz_dict.imat")


// Check indices
newdict(0->2)
newdict(csrow("<xx>","<posts>"))



// use getBeginEnd - to get indices of <xmltag> and </xmltag>
// col 0 has index imediately after <xmltag> and col1 has index of </xmltag>
// this is so that you can do elcol0->elcol1 and this gives the content inside
// if not desirable, use argument shiftBegInd=False

val postsIdx = getBeginEnd(xmlFile, newdict, "posts");
val postIdx = getBeginEnd(xmlFile, newdict, "post");
val eventIdx = getBeginEnd(xmlFile, newdict, "event");
val moodIdx = getBeginEnd(xmlFile, newdict, "current_moodid");


var nrUsers = 0
if (postsIdx(0,0) != -1) {nrUsers = size(postsIdx,1)}
// getBeginEnd3 returns irow(-1) \ irow(-1) if number of open/close xmltags is different
val sringIdx = getBeginEnd(xmlFile, newdict, "string");

val mm = getBeginEnd(xmlFile(moodIdx(0,0)->moodIdx(0,1)), newdict, "post")
size(mm) //empty

val mm = getBeginEnd(xmlFile(moodIdx(0,0)->moodIdx(0,1)), newdict, "int")



//Get not a number values
val ind = xmlFile(moodIdx(1,0)+find(xmlFile(moodIdx(1,0)->moodIdx(1,1))>0))
newdict(ind)

//Get number values -> need to be converted
val neg = xmlFile(moodIdx(1,0)+find(xmlFile(moodIdx(1,0)->moodIdx(1,1))<0))
twoComplementToInt(neg)


// Get word count from one post
// Have eventIdx(i,0) for index of <event> + 1
// Have eventIdx(i,1) for index of </event>

// For each i, get <string> </string> since we don't care about other types
// Not all </strings> are processed correctly by xmltweet
var i=2; val strPostIdx = getBeginEnd(xmlFile(eventIdx(i,0)->eventIdx(i,1)), newdict, "string"); 
//Try i=7,9,64,... others

var i=64;
var strPostIdx = getBeginEnd(xmlFile(eventIdx(i,0)->eventIdx(i,1)), newdict, "string"); 
strPostIdx += eventIdx(i,0);
val ind2 = xmlFile(strPostIdx(0,0)+find(xmlFile(strPostIdx(0,0)->strPostIdx(0,1))>0)) 
//get indices/values that are words within this string post
val postWords = newdict(ind2).t

// creates BoW for one post
val v = unique(ind2);
var i = 0;

var vec=izeros(v.size(),1);

while (i < v.size()) {
      vec(i)=sum(ind2==v(i));
      i += 1;
}

// better to do this directly for the sparse matrix



val pp = xmlFile(moodIdx(1,0)+find(xmlFile(moodIdx(1,0)->moodIdx(1,1))<0))
val p = icol(-12)
val ppp = pp on pp on p on pp on p;
val newppp = twoComplementToInt(ppp);


// Now bag of words / histogram /sparse mat of ind2, or postWords
// Out to great some bag of words out of ind2 ?? HERE ? 


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

val nrWords = size(newdict.cstr,1)
val nrPosts = size(postsIdx,1)
//sum(newdict.counts)
//maxi(newdict.counts)
//Create sparse matrix with Posti (row) Wordsj (cols) and nrCountsPostiWordj (vals)

// there is no way to update a particular sparse matrix unless that index exists
// if it doesn't we need to reconstruct it again
// seems like the best is to create List of ii's, jj's, and vv's (using prepend .+:)
// after that convert


var ii=List[Int]();
var jj=List[Int]();
var vv=List[Int]();
// ii.+:=(3);
ii = ii.+:(2).+:(2).+:(3);
jj = jj.+:(1).+:(1).+:(1);
vv = vv.+:(1).+:(2).+:(1);

var newsparse = sparse(icol(ii), icol(jj), col(vv), nrPosts, nrWords);
full(newsparse)
newsparse(?,0\1\2)

// HERE


// DICT UNIONS
val xmlFile2 = loadIMat(dir+"zy.xml.imat");
val newdict2 = loadDict(dir+"zy_dict.sbmat", dir+"zy_dict.imat");
val dd = Dict.union(newdict, newdict2);
val m1 = newdict --> dd;
val m2 = newdict2 --> dd;

newdict2(2) == dd(m2(2))
val f = find(xmlFile2==2)
newdict2(xmlFile2(f(0))) == dd(m2(xmlFile2(f(0))))


val m = Dict.treeFlush(Array[Dict](newdict, newdict2));
val p = dd.trim(2);
val mp1 = dd --> p

val m = newdict(find(sum(sBoWposts.t)>20)) 
// slow -> wordcount only within valid <event>
// Idea: update newdict.counts with this, then trim
// After that we merge smaller dictionaries

