/* Scanner for Twitter XML with emoticons */

%{
  extern int checkword(char *);
  extern void addtok(int tok);
  extern int parsedate(char * str);
  extern int numlines;

%}

%option never-interactive
%option noyywrap

LETTER	   [a-zA-Z_]
DIGF	   [0-9][0-9][0-9][0-9]
DIGT	   [0-9][0-9]
DIGIT	   [0-9]
PUNCT	   [;:,.?!]
AMP        ("&"|"&amp;")

%%

"<base64>"[^<>]+"</base64>" /* eat up base64 tokens */

"&lt;"[^&<=]{1,15}"&gt;"
"&amp;lt;"[^&<=]{1,15}"&amp;&gt;" /* eat up some short, simple ampersand based html tags in text */

"&lt;"[^&<=]*"="[^&<]*"&gt;" /* eat up longer html tags, these usually contain = signs */

"&lt;!"[^&<]*"&gt;" /* eat up xml comment tags <!-- comment -->, onditional tags <!-[if condition]>, even when formatted incorrectly*/

{AMP}"lt;""/"?[wo]":"[^&<]*{AMP}"gt;" /* eat some Microsoft tags that show up regularly */

{AMP}"lt;""/"?st[0-9]":"[^&<]*{AMP}"gt;" /* eat some more Microsoft "smart tags" */

{AMP}"lt;option"[ ]+"selected"{AMP}"gt;" /* Eat up a common tag that appears in polls */

{AMP}"nbsp;" /* These are nonbreaking spaces, eat them */

"&#xD;" /* These are newlines, eat them */

{AMP}"quot;" /* Discard quotation marks */

[:;]"</string>" {
  int i;
  /* Copy yytext because unput() trashes yytext */
  char *yycopy = strdup( yytext );
  for ( i = yyleng - 1; i >= 1; --i )
    unput( yycopy[i] );
  unput( ' ' );
  unput( yycopy[0] );
  free( yycopy );
} /* Protect close tags from becoming emoticons by inserting a space */

"http"s?"://" {
int iv = checkword(yytext);
} /* Protect hyperlinks from becoming emoticons, because :/ is a common one */

{AMP}"lt;"|{AMP}"gt;" {
int iv = checkword(yytext);
} /* if one of these does show up outside an emoticon or html tag, tokenize it properly so the semicolon doesn't dangle into an emoticon */

{AMP}"amp;" /* Discard the remaining ampersands */

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

[:;=]-?[>)}PD/o\]\\] {
  int iv = checkword(yytext);
	  }

[(\[][:;8=] {
  int iv = checkword(yytext);
      }

[)(\]\[]-[:;8=] {
  int iv = checkword(yytext);
}

">"?:"-"?< {
    int iv = checkword(yytext);
      }

">"?[:;]-[(\[\{O] {
        int iv = checkword(yytext);
	  }

">"?[:;=][(\[\{O] {
  int iv = checkword(yytext);
	  }

[8:]-?[D] {
  int iv = checkword(yytext);
	  }

":-"\|{1,2}    {
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

[:;]"o)"    {
  int iv = checkword(yytext);
	  }

"8)"    {
  int iv = checkword(yytext);
	  }

":^)"    {
  int iv = checkword(yytext);
	  }

"XD"    {
  int iv = checkword(yytext);
      } /* This will give a warning because the expression is already caught,
        but is left to remind us that it's an emoticon*/

":|"    {
  int iv = checkword(yytext);
}

"^"[-_]"^"    {
  int iv = checkword(yytext);
}

("<"|"&lt;"|"&amp;lt;")"3"+    {
  yytext[0]='<';
  yytext[1]='3';
  yytext[2] = '\0';
  int iv = checkword(yytext);
} /* Heart emoticons <3 */

("<"|"&lt;"|"&amp;lt;")[\\/]"3"+    {
  yytext[0]='<';
  yytext[1]='/';
  yytext[2]='3';
  yytext[3] = '\0';
  int iv = checkword(yytext);
} /* Broken heart emoticons </3 */

{PUNCT}	  {
  int iv = checkword(yytext);
	  }

"..""."*  {
  char ell[] = "...";
  int iv  = checkword(ell);
	  }

[\n]	  {
	  numlines++;
	  if (numlines % 1000000 == 0) {
	  fprintf(stderr, "\r%05d lines", numlines);
      fflush(stderr);
	  }
	  }

.         {}

%%
