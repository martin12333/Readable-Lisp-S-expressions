// Draft BNF grammar for sweet-expressions
// (c) 2012 David A. Wheeler, released under "MIT" license.

// This is a very early draft, more of an experiment to investigate using ANTLR
// to help create a good BNF.  In input, use > for indent, < for dedent, | for same.

grammar sweet;

options {
  k = 1;
}
// Simple demo grammar.

start 	:	 t_expr;

// Here are special interpretation for certain sequences.  Define these first, to give them
// higher lexical precedence than other definitions.
GROUP	:	 '\\' '\\';
splice 	:	GROUP;
DOLLAR 	:	'$';  // A '$' by itself isn't an atom unless inside {}, (), or [].
sublist :	DOLLAR;
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
abbrevh 		:	APOSH | QUASIQUOTEH | UNQUOTE_SPLICEH | UNQUOTEH;

EOL 	:	 '\r' '\n'? | '\n' '\r'? ;
eolchar : '\n' | '\r';

// Names for single characters and abbreviations.
SPACE 	:	 ' ';
TAB 	:	 '\t';
BANG 	:	 '!';
SEMI    :	 ';';
PERIOD  :	 '.';

APOS 			:	'\'';
QUASIQUOTE 		:	'\`';
UNQUOTE_SPLICE 		:	',' '@';
UNQUOTE 		:	',';
abbrev_noh		: APOS | QUASIQUOTE | UNQUOTE_SPLICE | UNQUOTE ;
abbrev_all		: abbrevh | abbrev_noh;

// Whitespace & indentation names
ichar   : SPACE | TAB | BANG ; // indent char
hspace  : SPACE | TAB ;        // horizontal space


// noteolchar : ~ (CR | LF);
wspace  : hspace | eolchar ;

indent 	: ichar*; // This is by definition ambiguous with INDENT/DEDENT/SAME/BADDENT

lcomment : SEMI ('a'..'z')* ; // TODO

eol_comment_lines : lcomment? EOL;  // TODO

// eol_comment_lines : lcomment? eol (indent lcomment eol)*;  // TODO


// STUBS: These are bogus stubs for s-expressions, INDENT, DEDENT, etc.
INDENT 	:	'>';
DEDENT 	:	'<';
SAME 	:	'|';
NAME  :	('a'..'z'|'A'..'Z'|'_'|'\\') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'\\')*
    ;
NUMBER 	:	 ('0'..'9')+ ;
atom 	:	 NAME | NUMBER ;
list_contents 
	:	 atom (wspace+ list_contents)?
	| ; /* empty */
n_expr :	 abbrev_all* (atom | '(' list_contents ')' );
n_expr_first
	:	 abbrev_noh* (atom | '(' list_contents ')' );
// END STUBS

// The "head" is the production for 1+ n-expressions on one line; it will
// return the list of n-expressions on the line.
// It never reads beyond the current line (except within a block comment),
// so it doesn't need to keep track of indentation, and callers can depend on this.
// On entry all indentation/hspace must have already been read.  On return it will have
// consumed all hspace (spaces and tabs).
// On a non-tokenizing recursive descent parser, have it also read and determine
// if the n-expression is special (e.g., //) and have it return a distinct value if it is.

head 	:	n_expr_first hspace* after;

// The "after" production reads the rest of the expressions on a line, after the first one.
// Like head, it consumes any hspace before it returns.
// "after" is written this way so a non-tokenizing implementation can read an expression specially. E.G.,
// if it sees a period, read the expression directly and then see if it's just a period.
// Note that block comments and datum comments that don't begin a line (after indent) are consumed
after 	: PERIOD hspace+ n_expr hspace* /* improper list.  Error if n_expr at this point */
	| BLOCK_COMMENT hspace* after
	| DATUM_COMMENT_START hspace* n_expr hspace* after  // Ignore the next datum
	| n_expr hspace* after
	| /* empty: ; eol */;

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
        | hspace+ (n_expr | eol_comment_lines t_expr); // Recurse and try again.

// TODO???: How to handle #|...|# at top level when nothing else on line - needs to be skipped and completely ignored.



/*
INT :	'0'..'9'+
    ;

FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT
    ;
*/

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

CHAR:  '\'' ( ESC_SEQ | ~('\''|'\\') ) '\''
    ;

fragment
EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

fragment
HEX_DIGIT : ('0'..'9'|'a'..'f'|'A'..'F') ;

fragment
ESC_SEQ
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   UNICODE_ESC
    |   OCTAL_ESC
    ;

fragment
OCTAL_ESC
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UNICODE_ESC
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;
  
*/
