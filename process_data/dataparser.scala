// TO RUN WITH BIDMACH DO:
// $ /opt/BIDMach_1.0.0-full-linux-x86_64/bidmach utils.scala examples.scala

import utils._  // utility methods

import scala.io.Source

import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat}
import BIDMat.MatFunctions._

object featurizers {

	def increaseBuffer(m:IMat,n: Int): IMat = {m\iones(m.nrows,n)};

	def featurizeMoodID(indir: String, dictdir:String,outdir:String,fileListPath: String): Unit = {

			//    val indir = "/var/local/destress/tokenized2/";
			//    var dictdir = "/var/local/destress/tokenized/";
			//    val outdir = "/var/local/destress/featurized/";
			//    val fileListPath= indir+"fileList.txt";

			val MaxMb = 100; // Approx size of sparse mat save batches (uncompressed)
			val postPerMb = 1000; // (Very) approx nr of posts which take 1Mb of saved space. May change!

			val wordsPerPost = 400; // (Very) approx number of words per post. 
      val initialBuffer = MaxMb*postPerMb*wordsPerPost; // Start size of rowIndices and colIndices buffers
      val bufferIncrease = MaxMb*postPerMb*25; // Amount to increase buffers storing sparse matrix entries if they overfill

			val masterDict = loadDict(dictdir+"masterDict.sbmat",dictdir+"masterDict.dmat");

			// Get nrWords in master dictionary
			val nrWords = masterDict.cstr.nrows;

			//Get list of xml files from input file.
			var source = Source.fromFile(fileListPath);
			var fileList = source.getLines.toList;
			source.close();

			// Save various stats in these variables
			var nrUsers = 0; // Total number of users
			var nrValidPosts = 0; // Total number of posts with <event><string>...</string></event> form

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
				flip;

				// Print a status update so the user can see something is happening
				println(s"Currently featurizing ${line}.xml");

				// Get the current xml file data and original dictionary
				var xmlFile = izeros(0,0);
				try {
					xmlFile = loadIMat(indir+line+".xml.imat");
				} catch {
				case e: Exception => println("exception caught in ${line}: " + e);    
				}
				var xmlDict = loadDict(indir+line+"_dict.sbmat"); // only load words

				var intIndex = xmlDict("<int>"); // Find the index of <int> tag in xmlDict

				// Map from the native dictionary to merged dictionary
				val mapToMaster = xmlDict --> masterDict;

				// Indexes for <posts> and </posts> (which enclose all activity by one user)
				val usersIdx = getBeginEnd(xmlFile, xmlDict, "posts");
				// Indexes for <post> and </post> (which enclose each activity by a user)
				val postIdx = getBeginEnd(xmlFile, xmlDict, "post"); 
				// Indexes for <event> and </event> (which could be a text post)
				val eventIdx = getBeginEnd(xmlFile, xmlDict, "event"); 

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

				var posti: Int = 0; // iteration counter for while
				var userk: Int = 0;

				while (posti < validEvent.nrows ) {

					val postStart = valpostIdx(posti, 0);
					val postEnd   = valpostIdx(posti, 1);

					// Increment userk until the first index of the post is less
					// than the last index of the current user
					while (postStart > usersIdx(userk, 1)) userk += 1;

					// Get indices of "current_moodid" open and close tags 
					val moodIdx = getBeginEnd(xmlFile(postStart -> postEnd), xmlDict, "current_moodid");

					// Check to make sure the current_moodid exists and that it is an int
					if (moodIdx.nrows==1 && xmlFile(postStart+moodIdx(0,0)) == intIndex ) {

						val moodid = twoComplementToInt(xmlFile(postStart+moodIdx(0,0)+1))(0);

						// Add to "labels" -> userid, currentmoodId
						labels(0,denseEntryNumber) = userk+nrUsers
								labels(1,denseEntryNumber) = moodid;

						// Get the post text, discarding numbers       
						var postWordId = getWordsOnly(xmlFile, valEventIdx(posti,0), valEventIdx(posti,1));
						// Map the text to the masterDict
						postWordId = mapToMaster(postWordId);
						// Discard -1's corresponding to words which aren't in the masterDict
						postWordId = postWordId(find(postWordId >= 0));

						val nWords = postWordId.nrows;

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
						denseEntryNumber+=1;
						sparseEntryNumber+=nWords;

						// Write the features to a file once MaxMb*postPerMb posts are processed
						if (denseEntryNumber == MaxMb*postPerMb) {

							println(s"\nWriting batch $batchNumber to file.\n");

							// Compress the sparse matrices, saves about half the disk space
							saveSMat(outdir+"data"+s"$batchNumber"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber)));
							// Label IMats are very small, no reason to compress
							saveIMat(outdir+"data"+s"$batchNumber"+".imat", labels); 

							// Reset the index/col trackers
							denseEntryNumber=0;
							sparseEntryNumber=0;

							// Increment batch number
							batchNumber+=1;

						}

					}

					// Next Post
					posti += 1;
				}

				// Update nrUsers to include this file
				if (usersIdx.nrows > 0 && usersIdx(0,0) != -1) {nrUsers += usersIdx.nrows}

				println(s"Tokenized in ${flop._2}s");
			}

			// Save the leftover data that didn't make the size threshold
			println(s"\nWriting batch $batchNumber to file.\n");
			saveSMat(outdir+"data"+s"$batchNumber"+".smat.lz4", sparse(rowIndices(0 until sparseEntryNumber),colIndices(0 until sparseEntryNumber),iones(1,sparseEntryNumber)));
			saveIMat(outdir+"data"+s"$batchNumber"+".imat", labels(?,0 until denseEntryNumber)); 

	}

}
