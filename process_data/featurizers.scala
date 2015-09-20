// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._  // utility methods

import scala.io.Source
import scala.math

import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._

import util.control.Breaks._

object featurizers {

  // List of all the regexes which give emoticons in the flex file, as string.
  // Concatenated by "|" into a single regex.
  val emoticonRegexes = """[:;=]-?[>)}PD/o\]\\]"""+"|"+"""[(\[][:;8=]"""+"|"+"""[)(\]\[]-[:;8=]"""+"|"+""">?:-?<"""+"|"+
  """>?[:;]-[(\[\{O]"""+"|"+""">?[:;=][(\[\{O]"""+"|"+"""[8:]-?[D]"""+"|"+
  """:-\|{1,2}"""+"|"+""":@"""+"|"+"""D:<?"""+"|"+"""D[8=]"""+"|"+
  """:\'-?\("""+"|"+"""[:;]o\)"""+"|"+"""8\)"""+"|"+""":^\)"""+"|"+"XD"+"|"+""":\|"""+"|"+"""^[-_]^"""+"|"+"""</?3""";

  // Functions to help get the list of emoticons in a dictionary
  def fullyMatches(s: String, regEx: scala.util.matching.Regex): Boolean = (regEx unapplySeq s).isDefined;
  def findMatches(s: CSMat, regEx: scala.util.matching.Regex): IMat = {
    val sLength = s.length;
    var indices=izeros(sLength,1)
    var i=0;
    while(i<sLength) {
      if (fullyMatches(s(i),regEx)) indices(i)=1;
      i+=1;
    }
    find(indices);
  }

  // This function will increase the size of a buffer array m by n columns
  def increaseBuffer(m:IMat,n: Int): IMat = {m\izeros(m.nrows,n)};

  def featurizeMoodID(indir: String, dictdir:String,outdir:String,fileListPath: String,dictName: String="masterDict"): Unit = {

    //    val indir = "/var/local/destress/tokenized/";
    //    var dictdir = "/var/local/destress/tokenized/";
    //    val outdir = "/var/local/destress/featurized/";
    //    val fileListPath= indir+"fileList.txt";

    val MaxMb = 100; // Approx size of sparse mat save batches (uncompressed)
    val postPerMb = 1000; // (Very) approx nr of posts which take 1Mb of saved space. May change!

    val wordsPerPost = 400; // (Very) approx number of words per post.
    val initialBuffer = MaxMb*postPerMb*wordsPerPost; // Start size of rowIndices and colIndices buffers
    val bufferIncrease = MaxMb*postPerMb*25; // Amount to increase buffers storing sparse matrix entries if they overfill

    val masterDict = loadDict(dictdir+dictName+".sbmat",pad=false);

    // Get nrWords in master dictionary
    val nrWords = masterDict.cstr.nrows;

    //Get list of xml files from input file.
    var source = Source.fromFile(fileListPath);
    val fileList = source.getLines.toList;
    source.close();

    // Save various stats in these variables
    var nrUsers = 0; // Total number of users
    var nrPosts = 0; // Total number of posts
    var nrStringPosts = 0; // Total number of posts with <event><string>...</string></event> form
    var moodIDFreq = izeros(1,135); // Histogram of moodids

    val maxWordCount = 100000;
    var wordCountFreq = izeros(1,maxWordCount+1); // Histogram of word count per post before mapping to masterDict

    val maxDiscardCount = 100000;
    var discardCountFreq = izeros(1,maxDiscardCount+1); // Histogram of word count per post after mapping to masterDict

    var dictCountsNew = dzeros(nrWords,1); // Counts in the dictionary for post text only

    var usersWithPosts = irow(0,-1); // Entry 0 counts the number of users with string posts, entry 1 tracks the current user

    // Initialize buffers to store word locations in sparse matrix
    var rowIndices = izeros(1,initialBuffer);
    var colIndices = izeros(1,initialBuffer);

    var labels = izeros(2, MaxMb*postPerMb); // Dense IMat with (UserId, CurrentMoodId) - later do datetime+replycount

    // Keeps track of number of batches, increasing each time one is saved
    var batchNumber = 1;

    var sparseEntryNumber=0; // Stores the current index at which to write data in rowIndices, colIndices
    var denseEntryNumber=0; // Stores the current col in which to write data to labels, also current col in sparse matrix
                            //Go through list:
    for (line <- fileList) {
      tic;

      // Print a status update so the user can see something is happening
      println(s"Currently featurizing ${line}.xml");

      // Get the current xml file data and original dictionary
      val xmlFile = loadIMat(indir+line+".xml.imat");
      val xmlDict = loadDict(indir+line+"_dict.sbmat",pad=true); // only load words, pad to be consistent with xmltweet output

      val intIndex = xmlDict("<int>"); // Find the index of <int> tag in xmlDict

      // Map from the native dictionary to merged dictionary
      val mapToMaster = xmlDict --> masterDict;

      // Indexes for <posts> and </posts> (which enclose all activity by one user)
      val usersIdx = getBeginEnd(xmlFile, xmlDict, "posts");
      // Indexes for <post> and </post> (which enclose each activity by a user)
      val postIdx = getBeginEnd(xmlFile, xmlDict, "post");
      nrPosts+=postIdx.nrows;
      // Indexes for <event> and </event> (which could be a text post)
      val eventIdx = getBeginEnd(xmlFile, xmlDict, "event");

      // Valid posts contain <event><string>...</string></event>
      // If </string> is not parsed properly, the post is discarded
      val validEvent = find(xmlFile(eventIdx(?,1)-1) == xmlDict("</string>"));

      // Update total number of valid posts
      nrStringPosts += validEvent.nrows;

      // valEventIdx points to <string> and </string>
      val valEventIdx: IMat = if (eventIdx.nrows>0 && validEvent.nrows>0) eventIdx(validEvent, ?) + (1\ -1) else izeros(0,2);
      val valpostIdx: IMat = if (postIdx.nrows==eventIdx.nrows) postIdx(validEvent,?) else izeros(0,2); //assumes postIdx.nrows == eventIdx.nrows

      var posti: Int = 0; // iteration counter for while
      var userk: Int = 0;

      while (posti < validEvent.nrows ) {

        val postStart = valpostIdx(posti, 0);
        val postEnd   = valpostIdx(posti, 1);

        // Increment userk until the first index of the post is less
        // than the last index of the current user
        while (postStart > usersIdx(userk, 1)) userk += 1;

        // Track the number of users with at least one <event><string>...
        if(userk+nrUsers>usersWithPosts(1)) {
          usersWithPosts(0)+=1;
          usersWithPosts(1)=userk+nrUsers;
        }

        // Get indices of "current_moodid" open and close tags
        val moodIdx = getBeginEnd(xmlFile(postStart -> postEnd), xmlDict, "current_moodid");

        // Check to make sure the current_moodid exists and that it is an int
        if (moodIdx.nrows==1 && xmlFile(postStart+moodIdx(0,0)) == intIndex ) {

          val moodid = twoComplementToInt(xmlFile(postStart+moodIdx(0,0)+1))(0);

          // Check to make sure that it is actually a valid moodid, at least one isn't
          if (moodid>0 && moodid < 135 && moodid!=50 && moodid!=94) {

            moodIDFreq(moodid)+=1; // Increment the histogram array

            // Add to "labels" -> userid, currentmoodId
            labels(0,denseEntryNumber) = userk+nrUsers;
            labels(1,denseEntryNumber) = moodid;

            // Get the post text, discarding numbers
            var postWordId = getWordsOnly(xmlFile, valEventIdx(posti,0), valEventIdx(posti,1));

            wordCountFreq( min(postWordId.nrows,maxWordCount) )+=1; // Update histogram of word counts before discards

            // Map the text to the masterDict
            postWordId = mapToMaster(postWordId);
            // Discard -1's corresponding to words which aren't in the masterDict
            postWordId = postWordId(find(postWordId >= 0));

            val nWords = postWordId.nrows;

            discardCountFreq( min(nWords, maxDiscardCount) )+=1; // Update histogram of word counts after discards

            // Save indices and rows for sparse feature matrix from this post.
            // If buffer is full, increase buffer size permanently
            try {
              rowIndices(0,sparseEntryNumber until (sparseEntryNumber+nWords))=postWordId.t;
            } catch{
              //  						case oob: java.lang.IndexOutOfBoundsException => {println("Increasing buffer size.");
              case oob: java.lang.RuntimeException => {println(s"\nIncreasing buffer size from ${rowIndices.ncols/(MaxMb*postPerMb)} to ${(rowIndices.ncols+bufferIncrease)/(MaxMb*postPerMb)} words per post.\n");
                rowIndices=increaseBuffer(rowIndices,bufferIncrease);
                colIndices=increaseBuffer(rowIndices,bufferIncrease);
                rowIndices(0,sparseEntryNumber until (sparseEntryNumber+nWords))=postWordId.t;};
            }
            colIndices(0,sparseEntryNumber until (sparseEntryNumber+nWords))=iones(1,nWords)*denseEntryNumber;

            // Increment the index counters
            if (nWords>0) {
              denseEntryNumber+=1;
              sparseEntryNumber+=nWords;
            }

            // Write the features to a file once MaxMb*postPerMb posts are processed
            if (denseEntryNumber == MaxMb*postPerMb) {

              println(s"\nWriting batch $batchNumber to file.\n");

              // Compress the sparse matrices, saves about half the disk space
              saveSMat(outdir+"data"+f"$batchNumber%03d"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber),nrWords,MaxMb*postPerMb));
              // Label IMats are very small, no reason to compress
              saveIMat(outdir+"data"+f"$batchNumber%03d"+".imat", labels);

              // Update word counts for post text
              dictCountsNew += accum(rowIndices(0 until sparseEntryNumber).t,ones(sparseEntryNumber,1),nrWords,1);
              // Reset the index/col trackers
              denseEntryNumber=0;
              sparseEntryNumber=0;

              // Increment batch number
              batchNumber+=1;
              
            }

          }

        }

        // Next Post
        posti += 1;
      }

      // Update nrUsers to include this file
      if (usersIdx.nrows > 0 && usersIdx(0,0) != -1) {nrUsers += usersIdx.nrows}

      println(s"Featurized in ${toc}s\n");
    }

    // Save the leftover data that didn't make the size threshold
    println(s"\nWriting batch $batchNumber to file.\n");
    saveSMat(outdir+"data"+f"$batchNumber%03d"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber),nrWords,denseEntryNumber));
    saveIMat(outdir+"data"+f"$batchNumber%03d"+".imat", labels(?,0 until denseEntryNumber));

    // Update word counts from post text
    dictCountsNew += accum(rowIndices(0 until sparseEntryNumber).t,ones(sparseEntryNumber,1),nrWords,1);
    // Save word counts from post text without the first buffer entry
    saveDMat(outdir+dictName+".dmat",dictCountsNew);
    // Also save the dictionary strings to the outdir so that it can't get lost
    saveSBMat(outdir+dictName+".sbmat",SBMat(masterDict.cstr));
    
    // Print some data statistics
    println(s"\nThere are a total of $nrUsers users in the data set.");
    println(s"Of these, ${usersWithPosts(0)} have at least one <string> post.");
    println(s"There are a total of $nrPosts <post> fields in the xml files.");
    println(s"There are a total of $nrStringPosts <string> posts, ${100*(sum(moodIDFreq)(0)/nrStringPosts.toFloat)}% of which have a moodid tag.");

    saveIMat(outdir+"wordCountHistogram.imat",wordCountFreq); // histogram of words per post before discarding
    saveIMat(outdir+"wordCountHistogram2.imat",discardCountFreq); // histogram of words per post before discarding
    saveIMat(outdir+"moodIDHistogram.imat",moodIDFreq); //histogram of moodids
  }


  def featurizeMoodIDSentence(indir: String, dictdir:String,outdir:String,fileListPath: String): Unit = {

    //    val indir = "/var/local/destress/tokenized/";
    //    var dictdir = "/var/local/destress/tokenized/";
    //    val outdir = "/var/local/destress/featurized/";
    //    val fileListPath= indir+"fileList.txt";

    val MaxMb = 100; // Approx size of sparse mat save batches (uncompressed)
    val postPerMb = 5000; // (Very) approx nr of posts which take 1Mb of saved space. May change!

    val wordsPerPost = 400; // (Very) approx number of words per post.
    val initialBuffer = MaxMb*postPerMb*wordsPerPost; // Start size of rowIndices and colIndices buffers
    val bufferIncrease = MaxMb*postPerMb*25; // Amount to increase buffers storing sparse matrix entries if they overfill

    val masterDict = loadDict(dictdir+"masterDict.sbmat",dictdir+"masterDict.dmat");

    // Get nrWords in master dictionary
    val nrWords = masterDict.cstr.nrows;

    //Get list of xml files from input file.
    var source = Source.fromFile(fileListPath);
    val fileList = source.getLines.toList;
    source.close();

    // Save various stats in these variables
    var nrUsers = 0; // Total number of users
    var nrPosts = 0; // Total number of posts
    var nrStringPosts = 0; // Total number of posts with <event><string>...</string></event> form
    var moodIDFreq = izeros(1,135) // Histogram of moodids

    val maxWordCount = 500;
    var wordCountFreq = izeros(1,maxWordCount+1); // Histogram of word counts in posts before mapping to masterDict

    val maxDiscardCount = 500;
    var discardCountFreq = izeros(1,maxDiscardCount+1) // Histogram of word counts in posts after mapping to masterDict

    var usersWithPosts = irow(0,-1); // Entry 0 counts the number of users with string posts, entry 1 tracks the current user

    // Initialize buffers to store word locations in sparse matrix
    var rowIndices = izeros(1,initialBuffer);
    var colIndices = izeros(1,initialBuffer);

    var labels = izeros(3, MaxMb*postPerMb*2); // Dense IMat with (UserId, CurrentMoodId, PostID) - later do datetime+replycount

    // var sents = CSMat(1, MaxMb*postPerMb+10000);
    var sents = IMat(maxWordCount, MaxMb*postPerMb*2);

    // Keeps track of number of batches, increasing each time one is saved
    var batchNumber = 1;

    var sparseEntryNumber=0; // Stores the current index at which to write data in rowIndices, colIndices
    var denseEntryNumber=0; // Stores the current col in which to write data to labels, also current col in sparse matrix

    //Go through list:
    for (line <- fileList) {
      // tic;

      // Print a status update so the user can see something is happening
      println(s"Currently featurizing ${line}.xml");

      // Get the current xml file data and original dictionary
      val xmlFile = loadIMat(indir+line+".xml.imat");
      val xmlDict = loadDict(indir+line+"_dict.sbmat"); // only load words

      val intIndex = xmlDict("<int>"); // Find the index of <int> tag in xmlDict

      // Map from the native dictionary to merged dictionary
      val mapToMaster = xmlDict --> masterDict;

      // Indexes for <posts> and </posts> (which enclose all activity by one user)
      val usersIdx = getBeginEnd(xmlFile, xmlDict, "posts");
      // Indexes for <post> and </post> (which enclose each activity by a user)
      val postIdx = getBeginEnd(xmlFile, xmlDict, "post");
      nrPosts+=postIdx.nrows;
      // Indexes for <event> and </event> (which could be a text post)
      val eventIdx = getBeginEnd(xmlFile, xmlDict, "event");

      // Valid posts contain <event><string>...</string></event>
      // If </string> is not parsed properly, the post is discarded
      val validEvent = find(xmlFile(eventIdx(?,1)-1) == xmlDict("</string>"));

      // If don't want to discard just append
      // find(xmlFile(eventIdx(?,1)-1) == xmlDict("string"));

      // Update total number of valid posts
      nrStringPosts += validEvent.nrows;

      // valEventIdx points to <string> +1 and to </string>: do col(0)->col(1)
      val valEventIdx: IMat = if (eventIdx.nrows>0 && validEvent.nrows>0) eventIdx(validEvent, ?) + (1\ -1) else izeros(0,2);
      val valpostIdx: IMat = if (postIdx.nrows==eventIdx.nrows) postIdx(validEvent,?) else izeros(0,2); //assumes postIdx.nrows == eventIdx.nrows

      var posti: Int = 0; // iteration counter for while
      var userk: Int = 0;

      while (posti < validEvent.nrows ) {

        breakable {
	      val postStart = valpostIdx(posti, 0);
	      val postEnd   = valpostIdx(posti, 1);

	      // Increment userk until the first index of the post is less
	      // than the last index of the current user
	      while (postStart > usersIdx(userk, 1)) userk += 1;

          // Track the number of users with at least one <event><string>...
          if(userk+nrUsers>usersWithPosts(1)) {
            usersWithPosts(0)+=1;
            usersWithPosts(1)=userk+nrUsers;
          }

	      // Get indices of "current_moodid" open and close tags
	      val moodIdx = getBeginEnd(xmlFile(postStart -> postEnd), xmlDict, "current_moodid");

	      // Check to make sure the current_moodid exists and that it is an int
	      if (moodIdx.nrows==1 && xmlFile(postStart+moodIdx(0,0)) == intIndex ) {

	        val moodid = twoComplementToInt(xmlFile(postStart+moodIdx(0,0)+1))(0);

            // Check to make sure that it is actually a valid moodid, at least one isn't
	        if (moodid>0 && moodid < 135 && moodid!=50 && moodid!=94) {

	          moodIDFreq(moodid)+=1; // Increment the histogram array

	          // Get the post text, discarding numbers
	          var postWordId = getWordsOnly(xmlFile, valEventIdx(posti,0), valEventIdx(posti,1));

	          // wordCountFreq( min(postWordId.nrows,maxWordCount) )+=1; // Update histogram of word counts before discards

	          // Map the text to the masterDict
	          postWordId = mapToMaster(postWordId);
	          // Discard -1's corresponding to words which aren't in the masterDict
	          postWordId = postWordId(find(postWordId >= 0));

	          val nWords = postWordId.nrows;

              if(nWords == 0) {
                break;
              }

	          // discardCountFreq( min(nWords, maxDiscardCount) )+=1; // Update histogram of word counts after discards

	          // Save indices and rows for sparse feature matrix from this post.
	          // If buffer is full, increase buffer size permanently
	          try {
	            rowIndices(0,sparseEntryNumber until (sparseEntryNumber+nWords))=postWordId.t;
	          } catch{
	            //  						case oob: java.lang.IndexOutOfBoundsException => {println("Increasing buffer size.");
	            case oob: java.lang.RuntimeException => {println(s"\nIncreasing buffer size from ${rowIndices.ncols/(MaxMb*postPerMb)} to ${(rowIndices.ncols+bufferIncrease)/(MaxMb*postPerMb)} words per post.\n");
		          rowIndices=increaseBuffer(rowIndices,bufferIncrease);
		          colIndices=increaseBuffer(rowIndices,bufferIncrease);
		          rowIndices(0,sparseEntryNumber until (sparseEntryNumber+nWords))=postWordId.t;};
	          }

              var punctSpots = (postWordId == masterDict(".")) + (postWordId == masterDict("!")) + (postWordId == masterDict("?"));
              var sentID = cumsum(punctSpots);
              sentID(1 until sentID.length) = sentID(0 until sentID.length - 1);

	          colIndices(0,sparseEntryNumber until (sparseEntryNumber+nWords))= sentID.t + denseEntryNumber; //iones(1,nWords)*denseEntryNumber;

              var nSents = maxi(sentID)(0) + 1;
              var denseNums = irow(denseEntryNumber -> (denseEntryNumber + nSents));

              // Add to "labels" -> userid, currentmoodId
	          labels(0,denseNums) = userk+nrUsers;
	          labels(1,denseNums) = moodid;
              labels(2,denseNums) = posti;


              // var currSent = "";
              // var currID = 0;
              // var sentIx = 0;

              // for(sentIx <- 0 until sentID.length) {
              //   if(currID != sentID(sentIx)) {
              //     sents(0, currID + denseEntryNumber) = currSent;
              //     currID = sentID(sentIx);
              //     currSent = "";
              //   }
              //   currSent += masterDict(postWordId(sentIx));
              //   currSent += " ";
              // }
              // sents(0, currID + denseEntryNumber) = currSent;

              var currIx = 0;
              var currID = 0;

              for(sentIx <- 0 until sentID.length) {
                if(currID != sentID(sentIx)) {
                  currID = sentID(sentIx);
                  wordCountFreq( min(currIx,maxWordCount) )+=1; // Update histogram of word counts before discards
                  currIx = 0;
                }
                
                if(currIx < maxWordCount) {
                  sents(currIx, currID + denseEntryNumber) = postWordId(sentIx); // convert word ID from int to float, to store in sparse mat
                }
                currIx += 1;
              }
              wordCountFreq( min(currIx,maxWordCount) )+=1; // Update histogram of word counts before discards
              
	          // Increment the index counters
              if (nWords>0) {
                denseEntryNumber+=nSents;
                sparseEntryNumber+=nWords;
              }

	          // Write the features to a file once MaxMb*postPerMb posts are processed
	          if (denseEntryNumber >= MaxMb*postPerMb) {

	            println(s"\nWriting batch $batchNumber to file.");
                tic;

	            // Compress the sparse matrices, saves about half the disk space
	            saveSMat(outdir+"data"+s"$batchNumber"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber),nrWords,denseEntryNumber));
	            // Label IMats are very small, no reason to compress
	            saveIMat(outdir+"data"+s"$batchNumber"+".imat", labels(?, 0 until denseEntryNumber));
                saveSMat(outdir+"data"+s"$batchNumber"+"_sent.smat.lz4", sparse(sents(?, 0 until denseEntryNumber)));

                saveIMat(outdir+"wordCountHistogram.imat",wordCountFreq); // histogram of words per post before discarding

                sents = sents * 0;

	            // Reset the index/col trackers
	            denseEntryNumber=0;
	            sparseEntryNumber=0;

	            // Increment batch number
	            batchNumber+=1;

                println(s"Written in ${toc}s\n");
	          }

	        }


	      }
        }

	    // Next Post
	    posti += 1;
      }

      // Update nrUsers to include this file
      if (usersIdx.nrows > 0 && usersIdx(0,0) != -1) {nrUsers += usersIdx.nrows}

      // println(s"Featurized in ${toc}s\n");
    }

    // Save the leftover data that didn't make the size threshold
    println(s"\nWriting batch $batchNumber to file.\n");
    saveSMat(outdir+"data"+s"$batchNumber"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber),nrWords,denseEntryNumber));
    saveIMat(outdir+"data"+s"$batchNumber"+".imat", labels(?,0 until denseEntryNumber));

    // Print some data statistics
    println(s"\nThere are a total of $nrUsers users in the data set.");
    println(s"Of these, ${usersWithPosts(0)} have at least one <string> post.");
    println(s"There are a total of $nrPosts <post> fields in the xml files.");
    println(s"There are a total of $nrStringPosts <string> posts, ${100*(sum(moodIDFreq)(0)/nrStringPosts.toFloat)}% of which have a moodid tag.");

    saveIMat(outdir+"wordCountHistogram.imat",wordCountFreq); // histogram of words per post before discarding
                                                              // saveIMat(outdir+"wordCountHistogram2.imat",discardCountFreq); // histogram of words per post before discarding
    saveIMat(outdir+"moodIDHistogram.imat",moodIDFreq); //histogram of moodids
  }


  def featurizeByUser(indir: String, dictdir:String,outdir:String,fileListPath: String): Unit = {

    //    val indir = "/var/local/destress/tokenized/";
    //    var dictdir = "/var/local/destress/tokenized/";
    //    val outdir = "/var/local/destress/featurized/";
    //    val fileListPath= indir+"fileList.txt";

    val MaxMb = 150; // Approx size of sparse mat save batches (uncompressed)
    val usersPerMb = 13000; // (Very) approx nr of posts which take 1Mb of saved space. May change!
    val usersPerBatch = MaxMb*usersPerMb;

    val moodIDPerUser = 50; // (Very) approx number of moodIDs per user
    val initialBuffer = usersPerBatch*moodIDPerUser; // Start size of rowIndices and colIndices buffers
    val bufferIncrease = usersPerBatch*5; // Amount to increase buffers storing sparse matrix entries if they overfill

    //Get list of xml files from input file.
    var source = Source.fromFile(fileListPath);
    val fileList = source.getLines.toList;
    source.close();

    // Save various stats in these variables
    var nrUsers = 0; // Total number of users
    var nrPosts = 0; // Total number of posts

    var nrMoods = 0; // Total number of <current_mood> and <current_moodid> tags
    var nrMoodStrings = 0; // Total number of <current_mood> tags enclosing <string> tags
    var nrMoodIDs = 0; // Total number of current_moodid tags, whether valid integers or not

    var moodIDFreq = izeros(1,135); // Histogram of moodids

    val maxMoodIDCount = 1000;
    var moodIDCountFreq = izeros(1,maxMoodIDCount+1); // Histogram of the number of moodids per user

    // Initialize buffers to store word locations in sparse matrix
    var rowIndices = izeros(1,initialBuffer);
    var colIndices = izeros(1,initialBuffer);

    // Keeps track of number of batches, increasing each time one is saved
    var batchNumber = 1;

    var sparseEntryNumber=0; // Stores the current index at which to write data in rowIndices, colIndices
    var denseEntryNumber=0; // Stores the current col in sparse matrix

    //Go through list:
    for (line <- fileList) {
      tic;

      // Print a status update so the user can see something is happening
      println(s"Currently featurizing ${line}.xml");

      // Get the current xml file data and original dictionary
      val xmlFile = loadIMat(indir+line+".xml.imat");
      val xmlDict = loadDict(indir+line+"_dict.sbmat",pad=true); // only load words

      val strIndex = xmlDict("<string>"); // Find the index of <string> tag in xmlDict
      val intIndex = xmlDict("<int>"); // Find the index of <int> tag in xmlDict

      // Indexes for <posts> and </posts> (which enclose all activity by one user) (starts after <posts> tag)
      val usersIdx = getBeginEnd(xmlFile, xmlDict, "posts");

      var useri: Int = 0; // iteration counter for while

      while (useri < usersIdx.nrows ) {

	    val userStart = usersIdx(useri, 0);
	    val userEnd   = usersIdx(useri, 1);

	    if( userEnd > userStart+1 ) {

	      // This section keeps counts of the numbers of non-integer moods
	      var moodIdx = getBeginEnd(xmlFile(userStart -> userEnd), xmlDict, "current_mood")+userStart;
	      nrMoods += moodIdx.nrows;
	      moodIdx = moodIdx(find(xmlFile(moodIdx(?,0)) == strIndex),0)+1;
	      nrMoodStrings += moodIdx.nrows;

	      // Indexes for <current_moodid> and </current_moodid>
	      moodIdx = getBeginEnd(xmlFile(userStart -> userEnd), xmlDict, "current_moodid")+userStart;

	      if(moodIdx.nrows>0) {

	        nrMoodIDs+=moodIdx.nrows; // Count all moodids

	        // Only want integer mood_ids, reduce to a list of the indices pointing to the integers
	        moodIdx = moodIdx(find(xmlFile(moodIdx(?,0)) == intIndex),0)+1;
	        // Get the moodids then filter out invalid integers
	        var moodIDs = twoComplementToInt(xmlFile(moodIdx));
	        moodIDs = moodIDs(find(moodIDs>0));
	        moodIDs = moodIDs(find(moodIDs<135));
	        moodIDs = moodIDs(find(moodIDs!=50));
	        moodIDs = moodIDs(find(moodIDs!=94));

	        val nIDs = moodIDs.nrows;

	        // Update the histograms
	        moodIDCountFreq( min(nIDs,maxMoodIDCount) )+=1;

	        var counter = 0;
	        while( counter < nIDs ) {moodIDFreq(moodIDs(counter)) += 1;counter+=1;};

	        // Save indices and rows for sparse feature matrix from this user.
	        // If buffer is full, increase buffer size permanently
	        try {
	          rowIndices(0,sparseEntryNumber until (sparseEntryNumber+nIDs))=moodIDs.t;
	        } catch{
	          case oob: java.lang.RuntimeException => {println(s"\nIncreasing buffer size from ${rowIndices.ncols/(usersPerBatch)} to ${(rowIndices.ncols+bufferIncrease)/(usersPerBatch)} moodids per user.\n");
	            rowIndices=increaseBuffer(rowIndices,bufferIncrease);
	            colIndices=increaseBuffer(rowIndices,bufferIncrease);
	            rowIndices(0,sparseEntryNumber until (sparseEntryNumber+nIDs))=moodIDs.t;};
	        }
	        colIndices(0,sparseEntryNumber until (sparseEntryNumber+nIDs))=iones(1,nIDs)*denseEntryNumber;

	        // Increment the index counters
	        if (nIDs>0) {
	          denseEntryNumber+=1;
	          sparseEntryNumber+=nIDs;
	        }

	        // Write the features to a file once usersPerBatch posts are processed
	        if (denseEntryNumber == usersPerBatch) {

	          println(s"\nWriting batch $batchNumber to file.\n");

	          // Compress the sparse matrices, saves about half the disk space
	          saveSMat(outdir+"data"+s"$batchNumber"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber),135,usersPerBatch));

	          // Reset the index/col trackers
	          denseEntryNumber=0;
	          sparseEntryNumber=0;

	          // Increment batch number
	          batchNumber+=1;

	        } // Close if (denseEntryNumber == usersPerBatch)

	      } // if(moodIdx.nrows>0)

	    } // Close if( userEnd > userStart+1 )

	    // Next user
	    useri += 1;

      } // Close while (useri < usersIdx.nrows )

      // Update nrUsers to include this file
      if (usersIdx.nrows > 0 && usersIdx(0,0) != -1) {nrUsers += usersIdx.nrows}

      println(s"Featurized in ${toc}s\n");

    } // Close for (line <- fileList)

    // Save the leftover data that didn't make the size threshold
    println(s"\nWriting batch $batchNumber to file.\n");
    saveSMat(outdir+"data"+s"$batchNumber"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber),135,denseEntryNumber));

    // Print some data statistics
    println(s"\nThere are a total of $nrUsers users in the data set.");

    val nrValidMoodIDs = (batchNumber-1)*usersPerBatch+denseEntryNumber;
    println(s"Of these, ${nrValidMoodIDs} (${100.0*nrValidMoodIDs/nrUsers}%) have at least one valid integer moodid.");

    println(s"The total number of <current_mood> and <current_moodid> tags is ${nrMoodIDs+nrMoods}.");
    println(s"There are ${nrMoodStrings} custom moods specified by strings.")
    println(s"The total number of <current_moodid> tags is ${nrMoodIDs}. Of these, ${sum(moodIDFreq)} (${100.0*sum(moodIDFreq)/nrMoodIDs}%) are valid integers.")

    saveIMat(outdir+"moodIDCountHistogram.imat",moodIDCountFreq); // histogram of number of moodids per user
    saveIMat(outdir+"moodIDHistogram.imat",moodIDFreq); // histogram of moodids by integer
  } // Close def featurizeByUser

}
