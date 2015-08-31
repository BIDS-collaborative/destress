
import java.io.File
import java.nio.file.{Paths, Files}

val indir = "/var/local/destress/featurized_sent"

val listLabels = new File(indir).list.filter(_.endsWith(".imat")).filter(_.startsWith("data"));
val listSentences = new File(indir).list.filter(_.endsWith("_sent.smat.lz4")).filter(_.startsWith("data"))
val nrs = listLabels.map(c => c.split('.')(0)).map(c => c.drop(4)).sortBy(_.toInt); //.sortWith(_<_);

val nrFiles = nrs.length

val percTest = 0.2
val nrTestFiles = floor(0.2*1131)(0).toInt

java.util.Collections.shuffle(java.util.Arrays.asList(nrs: _*)) //shuffles nrs, there's better ways...

val testFiles = nrs.take(nrTestFiles)


// FUTURE: Do loop through all testIndices
// But for now just do one File: testFiles(0)



val labels = loadIMat(indir+"/data"+testFiles(0)+".imat");
val sentences = loadSMat(indir+"/data"+testFiles(0)+"_sent.smat.lz4");


// Get the value/probability for each Sentence / MoodId
// Note: MoodID 1 through 134, including 134 (without 0, 50 and 94)
var probMat = zeros(132, sentences.ncols)
var sentId = 0

// For each sentence - Get prob of all moods
while (sentId < sentences.ncols) {
    val currSentence = sentences(?, sentId);

    // Get score based on currSentence
    // TODO -> comes from testing currSentence in word2vec queries
    val score = 0.9*ones(132,1)  // TODO HERE

    probMat(?, sentId) = score

    sentId += 1
}



// With probMat: aggregate sentences' scores to have a score per post.
val (a,b,c) = uniquerows(labels.t) 
// a - unique, c - ex: c(0) has index of a corresponding to label(0)
// Thus: a(c(0),?)=labels(?,0) // check BIDMAT wiki

val nrPosts = maxi(c)(0)
var posti = 0

var postLabel = izeros(1, nrPosts)
var postProbHat = zeros(132, nrPosts)

while (posti < nrPosts) {    // not very efficient
      // There are posts from 0 to nrPosts
      // This gets their moodId
      val currMood = labels(1, find(c==posti)(0))
      postLabel(0, posti) = currMood

      // This gets probabilities from all the sentences in posti
      // probMat(?,find(c==0))      
      // size is: 132 x nrSentencesInPost
      // If the metric is max, then:
      val currProb = maxi(probMat(?,find(c==0)),2)
      // mean(probMat(?, find(c==0)), 2) -> gives mean along all sentences
      postProbHat(?, posti) = currProb

      posti += 1
}

// AT THIS POINT
// we have
// one label per post
// 132 probabilities per post

// LET ME DO AUC
// THEN TAKE MEAN OF AUC
val nrMoods = 132+1+2
val validMoodIdx = (1->50) \ (51->94) \ (95->nrMoods)
val postPLabel = oneHot(postLabel)(validMoodIdx,?)  // remove row 0, 50, 94

val rocRes = 100
val rr = roc2(postProbHat, full(postPLabel), 1-full(postPLabel), rocRes)
val aucvec = mean(rr)