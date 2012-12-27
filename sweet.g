// Draft BNF grammar for sweet-expressions
// (c) 2012 David A. Wheeler, released under "MIT" license.

// This is an early draft, in part as an experiment to investigate using ANTLR
// to help create a good BNF.  It presumes that there's a preprocessor that
// does indent processing, so while indent processing is on, indents are
// marked with INDENT, dedents with DEDENT (one for each dedent), or SAME, and
// totally-blank lines have their indentation consumed (and DEDENTs generated).
// In input, use > for indent, < for dedent, | for same.
// Thus, a valid input would be:
//   \\ b c
//   >e f
//   <
//
//   #! hello
//   |a
//   >b c
//   <

// TODO:
// - Handle (IBM's) NEL
// - Initial PERIOD on a line
// - Add actions
// - Note/generate errors, e.g., illegal indents, initial "!"
// - (Maybe) Define n-expr, etc.
// - (Maybe) Handle EOF in weird places.
// -  Check that it works for improper lists, etc.



grammar sweet;

options {
  k = 1;
}
// Simple demo grammar.

start 	:	 t_expr;

// Lexer

// Here are special interpretation for certain sequences.  Define these first, to give them
// higher lexical precedence than other definitions.
GROUP	:	 '\\' '\\';
DOLLAR 	:	'$';  // A '$' by itself isn't an atom unless inside {}, (), or [].
RESERVED_TRIPLE :	 '$$$';  // Reserved for future use.
RESTART :	'<\*';
RESTART_END:	'\*>';

APOSH 			:	'\'' (' ' | '\t');  // Apostrophe + horizontal space
QUASIQUOTEH 		:	'\`' (' ' | '\t');  // Quasiquote + horizontal space
UNQUOTE_SPLICEH 	:	',' '@' (' ' | '\t');  // unquote-splicing + horizontal space
UNQUOTEH 		:	',' (' ' | '\t');  // unquote-splicing + horizontal space

// \u000b is vertical tab (\v).  Take that, http://prog21.dadgum.com/76.html
fragment EOL_CHAR : '\n' | '\r' | '\f' | '\u000b';
fragment NOT_EOL_CHAR : (~ (EOL_CHAR));

LCOMMENT : 	 ';' NOT_EOL_CHAR* ;

BLOCK_COMMENT   // #| ... #|
    :   '#|'
        (options {greedy=false;} : (BLOCK_COMMENT | .))*
        '|#' {$channel=HIDDEN;}
    ;
DATUM_COMMENT_START 	:	'#;' ;

// SRFI-105 notes that "implementations could trivially support (simultaneously) markers
// beginning with #! followed by a letter (such as the one to identify support for curly-infix-expressions),
// the SRFI-22 #!+space marker as an ignored line, and the
// format #!/ ... !# and #!. ... !# as a multi-line comment."
SRFI_22_COMMENT		:	'#! ' NOT_EOL_CHAR* ;
SHARP_BANG_FILE	:	'#!' ('/' | '.')
        (options {greedy=false;} : .)*
        '!#' {$channel=HIDDEN;} ;
// The following matches #!fold-case , #!no-fold-case, #!sweet, and #!curly-infix.
SHARP_BANG_MARKER 	:	'#!' ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'-')*;

// EOL is extremely special.  After reading it, we'll need to read in any following
// indent characters (if indent processing is active) to determine INDENT/SAME/DEDENT.
// As part of tokenizing, we'll consume any following lines that are ;-only lines.
fragment EOL_SEQUENCE : ('\r' '\n'? | '\n' '\r'?);
fragment BLANK_LINE 
	:	 (' ' | '\t')* ';' NOT_EOL_CHAR* | ('\f' | '\u000b') ;
EOL 	:	 ('\f' | '\u000b')? EOL_SEQUENCE
		 ( BLANK_LINE EOL_SEQUENCE)* ;

// Do not reference '\n' or '\r' inside a non-lexing rule.  ANTLR will quietly create
// lexical tokens for them if you do, and this will interfere with EOL processing.
// E.G., don't do:
//   eolchar : '\n' | '\r';

// Simple character or two-character sequences
SPACE 	:	' ';
TAB 	:	'\t';
SHARP	:	'#';
BANG 	:	'!';
PERIOD  :	'.';
LPAREN	:	'(';
RPAREN	:	')';
LBRACKET:	'[';
RBRACKET:	']';
LBRACE	:	'{';
RBRACE	:	'}';
APOS 			:	'\'';
QUASIQUOTE 		:	'\`';
UNQUOTE_SPLICE 		:	',' '@';
UNQUOTE 		:	',';



// STUBS: These are bogus stubs for s-expressions, INDENT, DEDENT, etc.
INDENT 	:	'>' ' '*;
DEDENT 	:	'<' ' '*;
SAME 	:	'|' ' '*;
// The following is intentionally limited.  In particular, it doesn't include
// the characters used for INDENT/DEDENT/SAME.
NAME  :	('a'..'z'|'A'..'Z'|'_'|'\\') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'\\')* ;
fragment EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;
FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT
    ;
INT 	:	 ('0'..'9')+ ;
fragment HEX_DIGIT : ('0'..'9'|'a'..'f'|'A'..'F') ;
fragment OCTAL_ESC
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;
fragment UNICODE_ESC
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;
fragment ESC_SEQ
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   UNICODE_ESC
    |   OCTAL_ESC
    ;

STRING:  '\"' ( ESC_SEQ | ~('\"'|'\\') ) '\"' ;
CHAR	:	'#' '\\' ('!'..'@' | '['..'`' | '{'..'~' | ('A'..'Z' | 'a'..'z')+);

atom 	:	 NAME | INT | FLOAT | STRING | CHAR;

list_contents 
	:	 atom (wspace+ list_contents)?
	| ; // empty

n_expr_tail 
	:	 (LPAREN list_contents RPAREN | LBRACE list_contents RBRACE |
		  LBRACKET list_contents RBRACKET) n_expr_tail?;
n_expr_noabbrev 
	:	 (atom | LPAREN list_contents RPAREN
		   | LBRACE list_contents RBRACE | LBRACKET list_contents RBRACKET)
		 /* TODO: n_expr_tail? */ ;
n_expr :	 abbrev_all* n_expr_noabbrev;
n_expr_first:	 abbrev_noh* n_expr_noabbrev;

// END STUBS



abbrevh 		:	APOSH | QUASIQUOTEH | UNQUOTE_SPLICEH | UNQUOTEH;
abbrev_noh		: APOS | QUASIQUOTE | UNQUOTE_SPLICE | UNQUOTE ;
abbrev_all		: abbrevh | abbrev_noh;
splice 	:	GROUP;  // Use this synonym to make its purpose clearer.
sublist :	DOLLAR; // Use this synonym to make its purpose clearer.

// Whitespace & indentation names
ichar   : SPACE | TAB | BANG ; // indent char
hspace  : SPACE | TAB ;        // horizontal space

wspace  : hspace;  // or eolchars

// Special comment - comment regions other than ";"
sharp_bang_comments 
	:	SRFI_22_COMMENT	| SHARP_BANG_FILE | SHARP_BANG_MARKER;

scomment:	BLOCK_COMMENT | DATUM_COMMENT_START hspace* n_expr | sharp_bang_comments;

// indent 	: ichar*; // This is by definition ambiguous with INDENT/DEDENT/SAME/BADDENT

// Read in ;comment (if exists), followed by EOL.  EOL consumes
// additional comment-only lines (if any).  On a non-tokenizing parser,
// this may reset indent as part of EOL processing.
comment_eol : LCOMMENT? EOL;

// The "head" is the production for 1+ n-expressions on one line; it will
// return the list of n-expressions on the line.
// It never reads beyond the current line (except within a block comment),
// so it doesn't need to keep track of indentation and indentation will NOT change within
// head; callers can depend on this.
// On entry all indentation/hspace must have already been read.  On return it will have
// consumed all hspace (spaces and tabs).
// On a non-tokenizing recursive descent parser, have it also read and determine
// if the n-expression is special (e.g., //, $, #!...!#, abbreviation + hspace)
// and have it return a distinct value if it is.

head 	:	n_expr_first (hspace+ rest?)?;

// The "rest" production reads the rest of the expressions on a line ("rest of the head"),
// after the first expression of the line.
// Like head, it consumes any hspace before it returns.
// "rest" is written this way so a non-tokenizing implementation can read an expression specially. E.G.,
// if it sees a period, read the expression directly and then see if it's just a period.
// Note that unlike the first head expression,
// block comments and datum comments that don't begin a line (after indent) are consumed,
// and abbreviations followed by a space merely apply to the next n-expression (not to the entire
// indented expression).
rest 	: PERIOD hspace+ n_expr hspace* /* improper list.  Error if n_expr at this point */
	| scomment hspace* rest?
	| n_expr (hspace+ rest?)?;

restart_list 
	:	RESTART hspace* comment_eol? (i_expr comment_eol)+ hspace* RESTART_END hspace*;

// body handles the sequence of 1+ child lines in an i_expr (e.g., after a "head").
// Note that DEDENT can't happen immediately after i_expr, because i_expr would consume it.
// Non-tokenizing implemenation notes:
// Note that i_expr will consume any line comments (line comments after
// content, as well as lines that just contain indents and comments).
// Note also that i-expr may set the the latest indent to a different value
// than the indent used on entry to body; the latest indent is compared by
// the special terminals DEDENT, SAME, and BADDENT.
body 	:	 i_expr (SAME body | DEDENT);

i_expr : head ( splice hspace* (i_expr | comment_eol (INDENT body)?)
              | DOLLAR hspace* (i_expr | comment_eol (INDENT body)?)
              | restart_list (i_expr | comment_eol (INDENT body)?)
              | comment_eol (INDENT body)?
              ) // child lines
         | (GROUP | scomment) hspace*
             (i_expr /* skip */
              | comment_eol (INDENT body | SAME i_expr | DEDENT /* error */ ))
         | DOLLAR hspace* (i_expr | comment_eol INDENT body)
	 | restart_list (i_expr | comment_eol (INDENT body)?)
         | abbrevh hspace* i_expr;

t_expr 	: i_expr
	| comment_eol t_expr
        | hspace+ (n_expr /* indent processing disabled */
                   | comment_eol t_expr /* try again */
                   | BANG /* error */)
        | BANG /* error */
        | EOF;


// Other ANTLR examples:
// COMMENT
//    :   '//' ~('\n'|'\r')* '\r'? '\n' {$channel=HIDDEN;}
//    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
//    ;

/*
WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) {$channel=HIDDEN;}
    ; 
*/
