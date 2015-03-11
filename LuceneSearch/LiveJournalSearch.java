import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopScoreDocCollector;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;

import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Scanner;
import java.util.HashMap;
import java.util.Map;

/* 
 *  Rough search implementation using Lucene's API w/ inverted index
 *  Index directory is located at file specified by INDEX_PATH_STR.
 *  Reads in a csv file and maps corresponding integer to mood strings
 * 
 *
 *  EXAMPLE SEARCH:
 *  (searches in the body-text field, for the string 'I am pregnant', with
 *   with a limit of 15 matches)
 *
 *  Enter which field to search or END to stop searching: ptext
 *  Query? I am pregnant
 *  Maxmimum number of searches? 15  
 *  
 *  Useful Query Syntax:
 *  'this is a string' : searches for the exact string  
 *  'this is*'         : searchs for the specified string PLUS a wildcard (generally more useful)
 *      Lucene Parser DOES NOT recognize '*' wildcard by itself. Do not preceed or follow it with a whitespace.
 *      Simply attach it to a string as demonstrated above.
 * 
 *  This searches for the string specified by query inside a FIELD.
 *    Possible Fields are:
 *      path : path to the specific .xml document
 *      ptext: the text body (what the user writes in the blog post)
 *      url  : the url to the actual blog entry, as in LiveJournal
 *      user : the username of the poster
 *      mood : integer value that evalutes to user-specifed moodid tag
 *  
 *  -----TO RUN:--------
 *  1) connect to mercury, cd to /home/geneyoo/LJ_Lucene
 *  2) compile with:
 *     javac -classpath .:lucene-core-4.0.0.jar:lucene-analyzers-common-4.0.0.jar:lucene-queryparser-4.0.0.jar LiveJournalSearch.java 
 *  3) run with:
 *     java -classpath .:lucene-core-4.0.0.jar:lucene-analyzers-common-4.0.0.jar:lucene-queryparser-4.0.0.jar LiveJournalSearch
 *  4) Follow console prompts.
 *  5) Results will get dumped to the console
 *
 * @author Gene Yoo
*/

public class LiveJournalSearch {

  public static final String INDEX_PATH_STR = "/var/local/destress/lj-annex/data/index";
  public static final String CSV_PATH_STR = "/home/geneyoo/LJ_Lucene/moodID.csv";


  /** 
   * Reads and parses the CSV file to a hashmap.
   * Each integer value is mapped to a unique mood string.
   * @param pathOfCSV      the string of path to the csv file
  */
  protected static void readCSV(String pathOfCSV) throws IOException, FileNotFoundException {
    BufferedReader csvReader = new BufferedReader(new FileReader(pathOfCSV));
    int moodID;
    String moodStr;
    String line = null;

    moodMap.put(-1, "N/A");
    while ((line = csvReader.readLine()) != null) {
      String[] parts = line.split(",");
      moodID = Integer.parseInt(parts[0]);
      moodStr = parts[1];
      moodMap.put(moodID, moodStr);
    }
    csvReader.close();
  }

  /** 
   * Prints the results of the search. 
   * @param field     the specified field to search
   * @param query     the specified query to search 
   * @param matches   the matches after performing the search
   * @param searcher  the searcher over the inverted indices
  */
  protected static void printResults(String field, String query,
                   ScoreDoc[] matches, IndexSearcher searcher) throws IOException {
    System.out.println("\n\nSearch results for QUERY = \"" + query + "\"");
    for (int i = 0; i < matches.length; i += 1) {
      int docID = matches[i].doc;
      Document d = searcher.doc(docID);
      int moodInt = Integer.parseInt(d.get("mood")); 

      System.out.println((i + 1) + ". Doc ID: " + docID);
      System.out.println("\tMood ID : " + d.get("mood"));
      System.out.println("\tMood    : " + moodMap.get(moodInt));
      System.out.println("\tText    : " + d.get("ptext"));
      System.out.println("\tUser ID : " + d.get("user"));
      System.out.println("\tURL     : " + d.get("url"));
      System.out.println("\tDoc Path: " + d.get("path"));
    }
  }

  /** 
   *  performSearch prompts user for maximum number of results, field to search,
   *  and the query text to search for. 
   *  It then searches through the index directory, and calls printResults
   *  to output the results of the search
   *  @param pathOfIndex    the path of the index directory 
   *  @return               true to keep searching, false otherwise.
  */

  protected static Boolean performSearch(String pathOfIndex) throws IOException, ParseException {
    
    File indexPath = new File(pathOfIndex);
    StandardAnalyzer analyzer = new StandardAnalyzer(Version.LUCENE_40);
    Directory index = FSDirectory.open(indexPath);

    Scanner userSC = new Scanner(System.in);
    int maxResults;
    String fieldStr, queryStr;

    System.out.print("Possible fields: ptext, user, path, url\n");
    System.out.print("Enter which field to search or END to stop searching: ");
    fieldStr = userSC.nextLine();

    if (fieldStr.equals("END") || fieldStr.equals("end")) {
      return false;
    }

    System.out.print("Query? ");
    queryStr = userSC.nextLine();

    System.out.print("Maxmimum number of searches? ");
    maxResults = userSC.nextInt();

    Query q = new QueryParser(Version.LUCENE_40, fieldStr, analyzer).parse(queryStr);

    IndexReader reader = DirectoryReader.open(index);
    IndexSearcher searcher = new IndexSearcher(reader);
    TopScoreDocCollector collector = TopScoreDocCollector.create(maxResults, true);
    searcher.search(q, collector);
    
    ScoreDoc[] matches = collector.topDocs().scoreDocs;
    
    printResults(fieldStr, queryStr, matches, searcher);
    reader.close();
    return true;
  }

  public static void main(String[] args) throws IOException, ParseException {

    Boolean searching = true;
    readCSV(CSV_PATH_STR);
    
    while (searching) {
        searching = performSearch(INDEX_PATH_STR);
    }
  }

  /* 
   * Map from integer value to corresponding String
   */
  protected static Map<Integer, String> moodMap = new HashMap<Integer, String>();
}
