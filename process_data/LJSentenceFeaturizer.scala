package BIDMach
import BIDMat.{Mat,SBMat,CMat,CSMat,Dict,DMat,FMat,GMat,GIMat,GSMat,HMat,IDict,IMat,SMat,SDMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.switch
import SentenceFeaturizer._
import java.io._
import scala.io.Source

class SentenceFeaturizer(val opts:SentenceFeaturizer.Options = new SentenceFeaturizer.Options) {
  	
  var masterDict:Dict = null
  
  def fileExists(fname:String) = {
    val testme = new File(fname)
    testme.exists
  }  
  
  def featurize(scanner:Scanner=LJScanner) = {
    println("Featurizing in " + opts.featDirName)
    
    if(opts.verbose>0) println("Loading masterDict.")
    if (masterDict == null) masterDict = Dict(HMat.loadSBMat(opts.mainDict))
    if(opts.verbose>0) println("Hashing masterDict.")
    masterDict.makeHash
    
    val nthreads = math.min(opts.nthreads, math.max(1, Mat.hasCUDA))
    val done = izeros(nthreads,1) // List of completion flags for each thread
    
    // Check if output directory exists, create if not
    val fd = new File(opts.featDirName) 
    if (!fd.exists) fd.mkdirs
    
    // Get list of files
    if(opts.verbose>0) println("Reading file list.")
    var source = Source.fromFile(opts.fileList);
    val fileList = source.getLines();
    
    for (ithread <- 0 until nthreads) {
      Future {

        if(opts.verbose>0 && nthreads>1) println(f"Initializing thread ${ithread}")
        if (Mat.hasCUDA > 0) setGPU(ithread+Mat.hasCUDA-nthreads)
        
        val unigramsx = IMat(2,opts.guessSize)
        val featuresx = IMat(2,opts.guessSize)
        var nfeats: Int = 0
        var nfiles: Int = 0
        var dict:Dict = null
        var map:IMat = null
        
      	for (file <- fileList) {

      	  if(opts.verbose>1 && nthreads>1) println(f"Featurizing tokenized file $file in thread $ithread")
          else if(opts.verbose>1) println(f"Featurizing $file")
          
          val fn = opts.fromFile(file)
      		val fdict = opts.tokDirName + file + opts.localDict

      		if (fileExists(fn) && fileExists(fdict)) {
      		  dict = Dict(loadSBMat(fdict))
      		  map = dict --> masterDict
 
            val idata = loadIMat( fn )
      		  if(opts.verbose>3) println(f"Scanning file $file")
      		  val (nuni, nsents) = scanner.scan(opts, dict, idata, unigramsx)
                
      		  if(opts.verbose>3) println(f"Mapping sent feats from file $file to overall dictionary.")
      		  // Map to overall dictionary, keeping OOV words as -1
      		  unigramsx(1,0->nuni)=map(unigramsx(1,0->nuni))
                
      		  // Add to sentence buffer
      		  if(opts.verbose>3 && nthreads>1) println(f"Adding ${nuni} words to buffer $ithread. Buffer is ${(nfeats.toFloat+nuni)/opts.guessSize.toFloat*100f}%% full.")
      		  if(opts.verbose>3 && nthreads==1) println(f"Adding ${nuni} words to buffer. Buffer is ${(nfeats.toFloat+nuni)/opts.guessSize.toFloat*100f}%% full.")

      		  if(nuni+nfeats>featuresx.ncols) println(f"ERROR: BUFFER $ithread OVERFLOW")
      		  else {
      		    featuresx(?,nfeats->(nfeats+nuni) )=unigramsx(?,0->nuni)
      		        nfeats+=nuni
      		  }

      		  // Save sentence buffer when enough data is available
      		  while(nfeats>opts.featsPerFile) {
      		    if(opts.verbose>3 && nthreads>1) println(f"Saving file $nfiles from buffer $ithread")
      		    if(opts.verbose>3 && nthreads==1) println(f"Saving file $nfiles")
      		    val fo = if(nthreads>1) opts.toSentFeatsPar(ithread,nfiles) else opts.toSentFeats(nfiles)
      		    saveIMat(fo,featuresx(?,0 until opts.featsPerFile))
      		    if(opts.verbose>3 && nthreads>1) println(f"Saved file $nfiles from buffer $ithread")
      		    if(opts.verbose>3 && nthreads==1) println(f"Saved file $nfiles from buffer")
      		    featuresx(0 until (nfeats-opts.featsPerFile))=featuresx(opts.featsPerFile until nfeats)
      		    nfeats -= opts.featsPerFile
      		    nfiles += 1
      		  }
      		  if(opts.verbose>3) println(f"Finished file $fn")
          } 
      		else println(f"Dict or file not found for $fn.")
        }

        done(ithread,0) = 1
      }
    }        
    while (mini(done).v == 0) Thread.`yield`
    
    source.close(); 
    
    saveMat(opts.featDirName+"masterDict.sbmat",masterDict.cstr)
  }
  
  
}

object SentenceFeaturizer {
  
  def alloptions = {
    val ff = new SentenceFeaturizer // Featurizer for non-emoticon posts
    ff
  }
  
  def buildFeatures() = {
    val ff = new SentenceFeaturizer
    ff.featurize() 
  }
  
  class Options {
    
    val tokDirName = "/var/local/destress/tokenized2/"
    val featDirName = "/var/local/destress/text_sent2/"
    
    val localDict:String = "_dict.sbmat"
    val localCount:String = "_dict.imat"
    
  	def mainDict:String = tokDirName + "masterDict.sbmat"
    def mainCounts:String = tokDirName + "masterDict.dmat"
    
    def fileList:String = tokDirName + "fileList.txt" 

    def fromFile:(String)=>String = (fn:String) => (tokDirName + fn + ".xml.imat")

    // Format for featurized file names in parallel
    def toSentFeatsPar:(Int,Int)=>String = (n:Int,m:Int) => (featDirName + "sentfeats%01d%05d.imat.lz4" format (n,m) )
    // Format for featurized file names in serial
    def toSentFeats:(Int)=>String = (n:Int) => (featDirName + "sentfeats%06d.imat.lz4" format n )

    var featsPerFile = 3*100000000/16
    
    var guessSize = 100000000
    var nthreads = 1
    
    var verbose = 1
  }
  
  trait Scanner {
    def scan(opts:SentenceFeaturizer.Options, dict:Dict, idata:IMat, unigramsx:IMat):(Int, Int)
  }
  
  object LJScanner extends Scanner {  
    final val OutsidePosts   = 0 // <posts> tag contains blocks of posts by user
    final val InsidePosts    = 1 
    final val InsidePost     = 2 // <post> tag contains individual posts
    final val InsideUser     = 3 // username is inside <user> tag
    final val InsideItemID   = 4 // <itemid> tag, usually (always?) <int>#</int> 
    // (Note: The ditemid value seems to be derived from userid and itemid as a crude way to prevent
    // bots from crawling by simply incrementing the itemid in the url)
    final val InsideEvent     = 5 // All posts are inside <event> tags
    final val InsideText     = 6 // Text posts have an <event><string> </string></event> format
    final val InsideTime     = 7 // e.g. <event_timestamp><int>1136720880</int></event_timestamp>
    final val InsideMoodID   = 8 // eg. <current_moodid><int>5</int></current_moodid>
    final val InsideMoodStr  = 9 // eg. <current_mood><string>wish i had my B.F. again!</string></current_mood>

    def scan(opts:SentenceFeaturizer.Options, dict:Dict, idata:IMat, unigramsx:IMat):(Int, Int) = {

      val Iintstart = dict("<int>")
      val Iintend = dict("</int>")
      val Istrstart = dict("<string>")
      val Istrend = dict("</string>")
        
      val Ipsstart = dict("<posts>")
      val Ipsend   = dict("</posts>")
      val Ipstart  = dict("<post>")
      val Ipend    = dict("</post>")
      val Iuser    = dict("<user>")
      val Iuend    = dict("</user>")
      val Iistart  = dict("<itemid>")
      val Iiend    = dict("</itemid>")
      val Iestart  = dict("<event>")
      val Ieend    = dict("</event>")
      val Itstart  = dict("<event_timestamp>")
      val Itend    = dict("</event_timestamp>")
      val Imidstart= dict("<current_moodid>")
      val Imidend  = dict("</current_moodid>")
      val Imstart  = dict("<current_mood>")
      val Imend   = dict("</current_mood>")

      val Ipunc   =  Array(dict("."),dict("!"),dict("?"),dict("...")) // Array of sentence breaking punctuation
    
      var state = 0

      var isent = 0 // Sentence id
      var nuni = 0
      var len = idata.length
      var i = 0
      while (i < len) {
        
        val tok = idata.data(i)-1 // Correct for indexing mismatch
            //			if (tok+1 >0) println(dict(tok)+ " " + state)
            //			else println("num " +(-(tok+1))+ " " + state)
        
        
        if (tok == Ipsend) {
          state = OutsidePosts
        } else {
              (state: @switch) match {
              case OutsidePosts => 
              if (tok == Ipsstart) {
                state = InsidePosts  
              }
              case InsidePosts => tok match {
                case Ipstart => state = InsidePost
                  case Ipsend => state = OutsidePosts
              }
              case InsidePost => tok match {
                  case Iuser   => state = InsideUser
                  case Iistart => state = InsideItemID
                  case Iestart =>	state = InsideEvent
                  case Itstart => state = InsideTime
                  case Imidstart=>state = InsideMoodID
                  case Imstart => state = InsideMoodStr
                  case _ => {}
              } 
              case InsideUser => tok match {
                  case Iuend   => state = InsidePost
                  case _ => {}
              }
              case InsideItemID => tok match {
                  case Iiend   => state = InsidePost
                  case _ => {}
              } 
              case InsideEvent => tok match {
                  case Ieend =>	state = InsidePost
                  case Istrstart => state = InsideText
                  case _ => {}
              }
              case InsideText => tok match {
                case Istrend => {
                  state = InsideEvent
                  isent += 1
                }
                // Safety for mis-tokenized </string>
                case Ieend => {
                  state = InsidePost 
                  isent+=1
                }
                case _ => if (tok+1 > 0) {
                  if (unigramsx != null) {
                    if (Ipunc contains tok) {
                      isent += 1
                      // Discard repeated end of sentence punctuation
                      while (Ipunc contains (idata.data(i+1)-1) ) {
                        i+=1
                      }
                    }
                    else {
                      unigramsx(0, nuni) = isent
                      unigramsx(1, nuni) = tok
                      nuni += 1
                    }
                  }
                }
              }
              case InsideTime => 
              tok match {
              case Itend =>	state = InsidePost
              case _ => {}
              }
              case InsideMoodID => 
              tok match {
              case Imidend =>	state = InsidePost
              case _ => {}
              } 
              case InsideMoodStr => 
              tok match {
              case Imend =>	state = InsidePost
              case _ => {}
              }
              case _ => {}
              }

            }
        i += 1
      }
      (nuni,isent)
    }
  }
}