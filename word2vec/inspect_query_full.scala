import java.io._
import scala.collection.mutable.ListBuffer

val results_dir = "/big/livejournal/full_results/user_studies/";
val text_dir = "/big/livejournal/full_results_text/user_studies/"
val top = 1000;

var svecs = Array(

  Array("neighbor"),
  Array("neighbor in my building"),
  Array("next door neighbor"),
  Array("well ... last night was uneventful ... really got to know the next door neighbor ",
    "saturday got up kinda early and went over the next door neighbor s house so i could meet cody and kayla"),
  Array("really got to know the next door neighbor ", "went over next door neighbor house"),
  Array("really got know next door neighbor", "next door neighbor house"),
  Array("my next door neighbor was screaming you like fucking with my stuff"),
  Array("the annoying neighbor from next door , jason , is moving out !"),

  // Coye
  Array("trust and emotion"),
  Array("exchange reciprocity"),
  Array("exchange reciprocity negotiation"),
  Array("personality and a mutual respect"),
  Array("family"),
  Array("holiday emotions"),
  Array("the night somehow fules my creativity and fills me with ideas and thoughts",
    "my feelings are actually always repressed ... i tend not to show them sometimes",
    "many fears are born of fatigue and loneliness"),

  // Victor
  Array("couple obese"),
  Array("eating obese"),
  Array("i m trying to eat healthy and avoid eating sweets"),
  Array("i lost weight and kept it off"),
  Array("i lost weight by using calorie counting"),
  Array("this other diet is better than calorie counting"),
  Array("I redesigned my lifestyle"),
  Array("I redesigned my lifestyle to lose weight"),
  Array("gobble eat devour"),
  Array("gobble devour"),
  Array("devour gobble ingest"),
  Array("slow carb diet"),
  Array("i am following atkins diet"),
  Array("i follow the paleo diet"),
  Array("yoyo diet"),
  Array("yo-yo diet"),
  Array("today begins a healthy eating and exercise regime"),
  Array("balancing better exercise and eating habits with my much loved unhealthy habits")
);



for(fnum <- 0 until svecs.length) {
  println(fnum);

  val pw = new PrintWriter(new File(text_dir + "/results_" + fnum + ".txt"));

  pw.write(svecs(fnum).mkString("\n"));
  pw.write("\n\n");

  var res = loadFMat(results_dir + "res_" + fnum + ".fmat");
  var sents = loadCSMat(results_dir + "sents_" + fnum + ".csmat.lz4");

  var (x, bestIndex) = sortdown2(res);

  var i = 0;
  var count = 0;
  var prev_score = -1f;

  while(count < top && i < bestIndex.length) {
    var ix = bestIndex(i);
    var score = res(ix);
    var sent = sents(ix);

    if(math.abs(prev_score - score) > 1e-7) { // remove duplicates
      prev_score = score;
      pw.write(f"$score%.3f -- $sent%s\n");
      count += 1;
    }

    i += 1;
  }

  pw.close();
}
