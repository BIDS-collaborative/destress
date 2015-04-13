import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.String;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileInputStream;
import java.io.InputStreamReader; 
import java.util.Arrays;
import java.util.ArrayList; 
import java.io.FileNotFoundException;
import java.io.IOException;

public class extract{

	public static void main(String [] args) throws FileNotFoundException,IOException  {
		String trainingdir = "/var/local/destress/";

		String filename = trainingdir + "google.txt";

		String words = "words.txt";
		String vectors = "vectors.txt";
		BufferedWriter word_writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(words)));
		BufferedWriter vector_writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(vectors)));
		FileInputStream fstream = new FileInputStream(filename);
		FileReader fr = new FileReader(filename); 
		BufferedReader br = new BufferedReader(fr);
		
		String line; 
		//Arraylist<String> arr = new Arraylist<String>(); 
		while ( (line = br.readLine()) != null) {
			String cur_line = line;
			String[] arr = cur_line.split(" ", 2); 
			String keyword = arr[0]; 
			String vecs = arr[1]; 
			word_writer.write(keyword + "\n");
			vector_writer.write(vecs + "\n");		
		}
		
	
		word_writer.close();
		vector_writer.close();
		br.close(); 	

			
	}
		//for (line <- Source.fromFile(filename).getLines()) {
		//  println(line)
		//  var cur_line = line;
		//  String array = cur_line.split(" ");
		//  String keyword = array(0);
		//  String vectors = array.slice(1,array.length);
		//
		//  word_writer.write(keyword + "\n")
		//  vector_writer.write(vectors + "\n")
		//}
}

