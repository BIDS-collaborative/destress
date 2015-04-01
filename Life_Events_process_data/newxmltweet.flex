/* Scanner for Twitter XML with emoticons */

%{
  extern int checkword(char *);
  extern void addtok(int tok);
  extern int parsedate(char * str);
  extern int numlines;

%}

%option never-interactive
%option noyywrap

LETTER              [a-zA-Z_]
DIGF                [0-9][0-9][0-9][0-9]
DIGT                [0-9][0-9]
DIGIT               [0-9]
PUNCT               [;:,.?!]

WORD                [a-zA-Z][a-zA-Z]*
PRONOUNS            "I"|"you"|"he"|"she"|"we"|"they"
SUPERLATIVE         "fucking"|"damn"|"awesome"|"very"|"super"|"so"|"incredibly"|"really"
NEGATIVESTATE       "pissed"|"mad"|"angry"|"sad"|"depressed"|"upset"|"dissapointed"
POSITIVESTATE       "happy"|"excited"|"energetic"|"enthusiastic"|"determined"|"diligent"|"engaged"|"hyper"
HAVE                "have"|"has"|"had"
NEED_TO             "need"\s"to"|"must"|"have"\s"to"|"got"\s"to"
POSSESIVE           "his"|"her"|"my"
HOPST               "doctor"|"nurse"|"surgeon"
CHANGE_PHRASE       "is"|"has"\s"been" + "getting"\s"better"|"getting"\s"worse"|"improving"|"growing"
DISEASE             "cancer"|"alzheimers"|"heart attack"|"stroke"|"sezuire"|"constipation"|"anseursym"|"sciatica"|"diabetes"|"tuberculosis"|"pneumonia"|"stroke"|"aids"|"hiv"|"add"|"adhd"|"arthritis"|"asthma"|"chlamydia"|"eplilepsy"|"flu"|"gonorrhea"|"hepatitis"|"meningitis"|"STD
DISEASE_ACT         "burst"\s"open"|"broke"|"is"\s"acting"\s"up"|"got"\s"worse"
HEALTH_STATE        "sick"|"ill"|"diabetic"|"anorexic"|"impaired"|"hospitalized"|"infected"|"nauseated"|"dizzy"|"healthy"|"strong"
BODY_PART           "leg"|"arm"|"elbow"|"ankle"|"fingers"
ORGAN               "lungs"|"kidney"|"bladder"|"pancreas"|"heart"|"arteries"|"ligaments"|"stomach"
DIET_NOUN           "weight"|"fat"|"mass"
DIET_STATE          "fat"|"skinny"|"healthy"|"overweight"|"obese"|"anorexic"|"small"|"lean"|"big"|"thin"|"bulky"|"heavy"
DIET_VERB           "eating"|"drinking"|"binging"|"pigging"|"gorging"

%%

{PRONOUN}\s["have"|"hate"]\s{SUPERLATIVE}?\s{DISEASE}                                              return A11;
{PRONOUN}\s["is"|"am"]\s{HEALTH_STATE}                                                             return A12;
{BODY_PART}\s{SUPERLATIVE}?\s{DISEASE_ACT}                                                         return A13;
{HOPST}\s"said"\s"that"\s{POSSESIVE}|"the"\s{DISEASE}|{BODY_PART}\s{DISEASE_ACT}|{CHANGE_PHRASE}   return A14;
{PRONOUN}\s["lost"|"gained"]\s"a"\s"lot"\s"of"\s{DIET_NOUN}                                        return A21;
{PRONOUN}\s{HAVE}\s{DIET_VERB}\s["problem"|"disorder"|"issue"]                                     return A22;
{PROUNOUN}\s["feel"|"am"|"look"]\s{SUPERLATIVE}?\s{DIET_STATE}                                     return A23;
{PRONOUN}\s{SUPERLATIVE}\s{NEED_TO}\s["lose"|"gain"]\s{DIET_NOUN}                                  return A24;



"<base64>"[^<>]+"</base64>" /* eat up base64 tokens */

[:;]"</string>" {
    int iv = checkword(yytext+1);
}


-?{DIGIT}+    {
#if __STDC_VERSION__ >= 199901L
  long long iv = strtoll(yytext, NULL, 10);
#else
  long iv = strtol(yytext, NULL, 10);
#endif
  addtok(iv);
  iv = iv >> 31;
  if (iv > 0 || iv < -1) {
    addtok(iv);
  }
}

-?{DIGIT}+"."{DIGIT}*   {
  float f = (float)strtod(yytext, NULL);
  int iv = *((int *)(&f));
  addtok(iv >> 1);
}

{DIGF}"-"{DIGT}"-"{DIGT}"T"{DIGT}":"{DIGT}":"{DIGT}("-"|"+"){DIGT}":"{DIGT}       {
  int tt = parsedate(yytext);
  addtok(tt);
}

{LETTER}+    {
  int iv = checkword(yytext);
  }

"<"{LETTER}+">"    {
    int iv = checkword(yytext);
  }

"</"{LETTER}+">"    {
  int iv = checkword(yytext);
  }

[:;]-[>)}] {
  int iv = checkword(yytext);
    }

[:;][>)}] {
  int iv = checkword(yytext);
    }

">"?:"-"?< {
    int iv = checkword(yytext);
}

">"?[:;]-[(\[\{O] {
        int iv = checkword(yytext);
    }

">"?[:;][(\[\{O] {
  int iv = checkword(yytext);
    }

[:8]-?[D] {
  int iv = checkword(yytext);
    }

":-||"    {
  int iv = checkword(yytext);
    }

":@"    {
  int iv = checkword(yytext);
    }

"D:""<"? {
  int iv = checkword(yytext);
    }

"D"[8=]   {
  int iv = checkword(yytext);
    }

":\'"-?"(" {
  int iv = checkword(yytext);
    }

":o)"    {
  int iv = checkword(yytext);
    }

"8)"    {
  int iv = checkword(yytext);
    }

":^)"    {
  int iv = checkword(yytext);
    }

{PUNCT}   {
  int iv = checkword(yytext);
    }

"..""."*  {
  char ell[] = "...";
  int iv  = checkword(ell);
    }

[\n]    {
    numlines++;
    if (numlines % 1000000 == 0) {
    fprintf(stderr, "\r%05d lines", numlines);
      fflush(stderr);
    }
    }

.         {}

%%
