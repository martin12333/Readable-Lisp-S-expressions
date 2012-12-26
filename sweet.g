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

// TODO:
// - Deal with #|...|# when nothing follows
// - Handle FORMFEED | VTAB | (IBM's) NEL
// - Handle #! as discussed in SRFI-105 (curly-infix), including SRFI-22
// - Handle #!sweet, #!curly-infix, #!fold-case, #!no-fold-case, etc.
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
BLOCK_COMMENT   // #| ... #|
    :   '#|'
        (options {greedy=false;} : (BLOCK_COMMENT | .))*
        '|#' {$channel=HIDDEN;}
    ;
DATUM_COMMENT_START 	:	'#;' ;
SHARP_BANG 		:	'#!' ;
APOSH 			:	'\'' (' ' | '\t');  // Apostrophe + horizontal space
QUASIQUOTEH 		:	'\`' (' ' | '\t');  // Quasiquote + horizontal space
UNQUOTE_SPLICEH 	:	',' '@' (' ' | '\t');  // unquote-splicing + horizontal space
UNQUOTEH 		:	',' (' ' | '\t');  // unquote-splicing + horizontal space
LCOMMENT : 	 ';' (~ ('\n' | '\r'))* ;

// EOL is extremely special.  After reading it, we'll need to read in any following
// indent characters (if indent processing is active) to determine INDENT/SAME/DEDENT.
// As part of tokenizing, we'll consume any following lines that are ;-only lines.
EOL 	:	 ('\r' '\n'? | '\n' '\r'?)
		 ((' ' | '\t')* ';' (~ ('\n' | '\r'))* ('\r' '\n'? | '\n' '\r'?))* ;

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
SAME 	:	'|';
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

CHAR:  '\'' ( ESC_SEQ | ~('\''|'\\') ) '\'' ;

atom 	:	 NAME | INT | FLOAT | CHAR;

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

// indent 	: ichar*; // This is by definition ambiguous with INDENT/DEDENT/SAME/BADDENT

// Read in ;comment (if exists), followed by EOL.  On a non-tokeninzing parser,
// this may reset indent as part of EOL processing.
eol_comment_lines : LCOMMENT? EOL;

// The "head" is the production for 1+ n-expressions on one line; it will
// return the list of n-expressions on the line.
// It never reads beyond the current line (except within a block comment),
// so it doesn't need to keep track of indentation and indentation will NOT change within
// head; callers can depend on this.
// On entry all indentation/hspace must have already been read.  On return it will have
// consumed all hspace (spaces and tabs).
// On a non-tokenizing recursive descent parser, have it also read and determine
// if the n-expression is special (e.g., //, $, #!...!#, abbreviation + hspace)
// and have it return a distinct value if it is,

head 	:	n_expr_first (hspace+ rest?)?;

// The "rest" production reads the rest of the expressions on a line ("rest of the head"),
// after the first expression of the line.
// Like head, it consumes any hspace before it returns.
// "rest" is written this way so a non-tokenizing implementation can read an expression specially. E.G.,
// if it sees a period, read the expression directly and then see if it's just a period.
// Note that block comments and datum comments that don't begin a line (after indent) are consumed
rest 	: PERIOD hspace+ n_expr hspace* /* improper list.  Error if n_expr at this point */
	| BLOCK_COMMENT hspace* rest
	| DATUM_COMMENT_START hspace* n_expr hspace* rest  // Ignore the next datum
	| n_expr (hspace+ rest?)?;

// body handles the sequence of 1+ child lines in an i_expr.
// Note that DEDENT can't happen immediately after i_expr, because i_expr would consume it.
// Non-tokenizing implemenation notes:
// Note that i_expr will consume any line comments (line comments after
// content, as well as lines that just contain indents and comments).
// Note also that i-expr may set the the latest indent to a different value
// than the indent used on entry to body; the latest indent is compared by
// the special terminals DEDENT, SAME, and BADDENT.
body 	:	 i_expr (SAME body | DEDENT);

i_expr : head ( splice hspace* (i_expr | eol_comment_lines (INDENT body)?)
              | DOLLAR hspace* (i_expr | eol_comment_lines (INDENT body)?)
              | eol_comment_lines (INDENT body)?
              ) // child lines
         | (GROUP | BLOCK_COMMENT) hspace* (i_expr | eol_comment_lines INDENT body)
         | DOLLAR hspace* (i_expr | eol_comment_lines INDENT body)
         | abbrevh hspace* i_expr;

t_expr 	: i_expr
	| eol_comment_lines t_expr
        | hspace+ (n_expr /* indent processing disabled */
                   | eol_comment_lines t_expr /* try again */
                   | BANG /* error */)
        | BANG /* error */
        | EOF;

// TODO???: How to handle #|...|# at top level when nothing else on line - needs to be skipped and completely ignored.


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
