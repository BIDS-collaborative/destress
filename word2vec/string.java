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
import java.lang.StringBuilder; 


public class string{
	
	public static void main(String [] args) throws FileNotFoundException,IOException {
		String trainingdir = "/var/local/destress/";
		String filename = trainingdir + "words.txt";
		String words = "oneLiner.txt";
		BufferedWriter word_writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(words)));

		FileReader fr = new FileReader(filename);
                BufferedReader br = new BufferedReader(fr);
		
		StringBuilder sb = new StringBuilder(1000); 
		String line; 
		while( (line = br.readLine()) != null) {
			sb.append(line); 
			sb.append(" "); 
		}
		word_writer.write(sb.toString());
		word_writer.close(); 
	}	





}
