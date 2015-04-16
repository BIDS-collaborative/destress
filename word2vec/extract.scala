import utils._   // utility methods 
import java.io.File 
import java.nio.file.{Paths, Files}
import scala.io.Source 
 
import BIDMat.{CMat, CSMat, DMat, Dict, FMat, FND, GMat, GDMat, GIMat, GLMat, GSMat, GSDMat, HMat, IDict, Image, IMat, LMat, Mat, SMat, SBMat, SDMat} 
import BIDMat.MatFunctions._ 
import BIDMat.SciFunctions._ 
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException



var trainingdir = "/var/local/destress/"; 

val filename = trainingdir + "google.txt";

val words = "words.txt";
val vectors = "vectors.txt";
val word_writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(words)))
val vector_writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(vectors)))

for (line <- Source.fromFile(filename).getLines()) {
  println(line)
  var cur_line = line;
  val array = cur_line.split(" ");
  val keyword = array(0); 
  val vectors = array.slice(1,array.length); 

  word_writer.write(keyword + "\n")
  vector_writer.write(vectors + "\n")
}
word_writer.close()
vector_writer.close() 
