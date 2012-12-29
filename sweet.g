// BNF grammar for sweet-expressions
// (c) 2012 David A. Wheeler, released under "MIT" license.

// Written using ANTLR version 3, single-token (k=1) lookahead. More info:
// http://www.antlr.org/

// This BNF presumes there's a preprocessor that
// does indent processing, so while indent processing is on, indents are
// marked with INDENT, dedents with DEDENT (one for each dedent), and
// totally-blank lines have their indentation consumed (and DEDENTs generated).
// If you use this BNF directly, for input, use > for indent, < for dedent.
// There is no "SAME" token (originally there was, with "|"); some rules use
// an empty "same" nonterminal to emphasize the lack of INDENT/DEDENT.
// Thus, a valid input would be:
//   a b
//   >c
//   d e
//   <
// to represent:
// a b
//   c
//   d e
//

// Actions are expressed as /*= ...Scheme code... */

// TODO:
//   Syntactically it works below, but it's more challenging to create
//   its actions as written below.
// - Ensure that at top level, "a \\ b c" produces "a" then "(b c)",
//   not "a", "b", "c".  Thus, must read hspace* after \\.  Easy way would be
//   to add (' ' | '\t')* to end of GROUP, to force consumption early.
// - (Maybe) Define n-expr, etc.
// - (Maybe) Handle EOF in weird places.

// Uses:
// ; Given pair, return its car if cdr is null, else the pair.
// ; Note that if x is improper, it just returns x.
// (define (monify x)
//   (if (null? (cdr x))
//     (car x)
//     x)

grammar sweet;

options {
  k = 1;  // This grammar is LL(1).
}
// Simple demo grammar.

start   :        t_expr;

// Lexer

// Here are special interpretation for certain sequences.  Define these first, to give them
// higher lexical precedence than other definitions.
GROUP   :        '\\' '\\';
DOLLAR  :       '$';  // A '$' by itself isn't an atom unless inside {}, (), or [].
RESERVED_TRIPLE_DOLLAR : '$$$';  // Reserved for future use.
RESTART :       '<\*';
RESTART_END:    '\*>';

// Abbreviations followed by horizontal space (space or tab) are special:
APOSH           : '\'' (' ' | '\t') ;
QUASIQUOTEH     : '\`' (' ' | '\t') ;
UNQUOTE_SPLICEH : ',' '@' (' ' | '\t') ;
UNQUOTEH        : ',' (' ' | '\t') ;

// End-of-line and comment handling:
fragment FF :    '\f'; // Formfeed 
fragment VT :   '\u000b';  // Vertical tab (\v).  Take that, http://prog21.dadgum.com/76.html
fragment NEL:   '\u0085'; // Hi, IBM mainframes!
fragment EOL_CHAR : '\n' | '\r' | FF | VT | NEL;
fragment NOT_EOL_CHAR : (~ (EOL_CHAR));

LCOMMENT :       ';' NOT_EOL_CHAR* ; // "Line comment"

BLOCK_COMMENT   // #| ... #|
    :   '#|'
        (options {greedy=false;} : (BLOCK_COMMENT | .))*
        '|#' {$channel=HIDDEN;}
    ;
DATUM_COMMENT_START     :       '#;' ;

// SRFI-105 notes that "implementations could trivially support (simultaneously) markers
// beginning with #! followed by a letter (such as the one to identify support for curly-infix-expressions),
// the SRFI-22 #!+space marker as an ignored line, and the
// format #!/ ... !# and #!. ... !# as a multi-line comment."
SRFI_22_COMMENT         :       '#! ' NOT_EOL_CHAR* ;
SHARP_BANG_FILE :       '#!' ('/' | '.')
        (options {greedy=false;} : .)*
        '!#' {$channel=HIDDEN;} ;
// The following matches #!fold-case , #!no-fold-case, #!sweet, and #!curly-infix.
SHARP_BANG_MARKER       :       '#!' ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'-')*;

// EOL is extremely special.  After reading it, we'll need to read in any following
// indent characters (if indent processing is active) to determine INDENT/DEDENT.
// As part of tokenizing, we'll consume any following lines that are ;-only lines.

// Support GNU Coding Standards (http://www.gnu.org/prep/standards/standards.html):
// "Please use formfeed characters (control-L) to divide the program into pages at logical places
// (but not within a function). It does not matter just how long the pages are, since they do not
// have to fit on a printed page. The formfeeds should appear alone on lines by themselves."
// Thus, FF and VT are supported, but they must be at the beginning of a line and do not themselves
// create a newline (they still have to be followed by an EOL_SEQUENCE).
fragment EOL_SEQUENCE : ('\r' '\n'? | '\n' '\r'? | NEL);
fragment BLANK_LINE 
        :        (' ' | '\t')* ';' NOT_EOL_CHAR* | (FF | VT)+ ;
EOL     :        (FF | VT)* EOL_SEQUENCE
                 ( BLANK_LINE EOL_SEQUENCE)* ;

// Do not reference '\n' or '\r' inside a non-lexing rule.  ANTLR will quietly create
// lexical tokens for them if you do, and this will interfere with EOL processing.
// E.G., don't do:
//   eolchar : '\n' | '\r';

// Simple character or two-character sequences
SPACE   :       ' ';
TAB     :       '\t';
SHARP   :       '#';
BANG    :       '!';
PERIOD  :       '.';
LPAREN  :       '(';
RPAREN  :       ')';
LBRACKET:       '[';
RBRACKET:       ']';
LBRACE  :       '{';
RBRACE  :       '}';
APOS                    :       '\'';
QUASIQUOTE              :       '\`';
UNQUOTE_SPLICE          :       ',' '@';
UNQUOTE                 :       ',';

empty   : ;  // Use this to emphasize empty branches
error   : ;  // Use this to identify error branches, to quickly id them
same    : ;  // Use this to emphasize neither indent nor dedent.



// STUBS: These are bogus stubs for s-expressions, INDENT, DEDENT, etc.
// REMOVE THESE STUBS from a formal BNF for SRFI, etc.
INDENT  :       '>' ' '*;
DEDENT  :       '<' ' '*;

// The following is intentionally limited.  In particular, it doesn't include
// the characters used for INDENT/DEDENT.
NAME  : ('a'..'z'|'A'..'Z'|'_'|'\\') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'\\')* ;
fragment EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;
FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT
    ;
INT     :        ('0'..'9')+ ;
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
CHAR    :       '#' '\\' ('!'..'@' | '['..'`' | '{'..'~' | ('A'..'Z' | 'a'..'z')+);

atom    :        NAME | INT | FLOAT | STRING | CHAR;

list_contents 
        :        atom (wspace+ list_contents)?
        | empty ;

n_expr_tail 
        :        (LPAREN list_contents RPAREN | LBRACE list_contents RBRACE |
                  LBRACKET list_contents RBRACKET);
n_expr_noabbrev 
        :        (atom | LPAREN list_contents RPAREN
                   | LBRACE list_contents RBRACE | LBRACKET list_contents RBRACKET)
                 (options {greedy=true;} : n_expr_tail)*;
// END STUBS


// To simplify ANTLRWorks debugger parse tree use, redefine indent/dedent as nonterminals
indent  : INDENT;
dedent  : DEDENT;

abbrevh : APOSH /*= 'quote */
        | QUASIQUOTEH /*= 'quasiquote */
        | UNQUOTE_SPLICEH /*= 'unquote-splicing */
        | UNQUOTEH /*= 'unquote */;
abbrev_noh              : APOS | QUASIQUOTE | UNQUOTE_SPLICE | UNQUOTE ;
abbrev_all              : abbrevh | abbrev_noh;
splice  :       GROUP;  // Use this synonym to make its purpose clearer.
sublist :       DOLLAR; // Use this synonym to make its purpose clearer.

// n_expr is a full neoteric-expression
n_expr :         abbrev_all* n_expr_noabbrev;
// n_expr_first is a neoteric-expression, but abbreviations cannot have an hspace afterwards
n_expr_first:    abbrev_noh* n_expr_noabbrev;

// Whitespace & indentation names
ichar   : SPACE | TAB | BANG ; // indent char
hspace  : SPACE | TAB ;        // horizontal space

wspace  : hspace;  // or eolchars

// Special comment - comment regions other than ";"
sharp_bang_comments 
        :       SRFI_22_COMMENT | SHARP_BANG_FILE | SHARP_BANG_MARKER;

scomment:       BLOCK_COMMENT | DATUM_COMMENT_START hspace* n_expr | sharp_bang_comments;

// indent       : ichar*; // This is by definition ambiguous with INDENT/DEDENT/BADDENT

// Read in ;comment (if exists), followed by EOL.  EOL consumes
// additional comment-only lines (if any).  On a non-tokenizing parser,
// this may reset indent as part of EOL processing.
comment_eol : LCOMMENT? EOL;

// The "head" is the production for 1+ n-expressions on one line; it will
// return the list of n-expressions on the line.  If there is one one, it returns
// a list of exactly one item; this makes it easy to append to later (if appropriate).
// It never reads beyond the current line (except within a block comment),
// so it doesn't need to keep track of indentation and indentation will NOT change within
// head; callers can depend on this.
// On entry all indentation/hspace must have already been read.  On return it will have
// consumed all hspace (spaces and tabs).
// On a non-tokenizing recursive descent parser, have it also read and determine
// if the n-expression is special (e.g., //, $, #!...!#, abbreviation + hspace)
// and have it return a distinct value if it is.

head    : n_expr_first
           ((hspace+ (rest /*= (cons $n_expr_first rest) */
                     | empty /*= (list $n_expr_first) */))
            | empty /*= (list $n_expr_first) */)
        | PERIOD
           (hspace+ n_expr hspace* /*= $n_expr */ (n_expr error)?
           | empty /*= '. */ ) ;

// The "rest" production reads the rest of the expressions on a line ("rest of the head"),
// after the first expression of the line.
// Like head, it consumes any hspace before it returns.
// "rest" is written this way so a non-tokenizing implementation can read an expression specially. E.G.,
// if it sees a period, read the expression directly and then see if it's just a period.
// Note that unlike the first head expression,
// block comments and datum comments that don't begin a line (after indent) are consumed,
// and abbreviations followed by a space merely apply to the next n-expression (not to the entire
// indented expression).
rest    : PERIOD hspace+ n_expr hspace* /* improper list. */
          /*= $n_expr */
          (n_expr error)? /* Shouldn't have another n_expr! */
        | scomment hspace* (rest /*= $rest */
                           | empty /*= '() */)
        | n_expr
            ((hspace+ (rest /*= (cons $n_expr) */
                       | empty /*= (list $n_expr) */))
              | empty /*= (list $n_expr) */) ;


// "body" handles the sequence of 1+ child lines in an i_expr (e.g., after a "head"),
// each of which is itself an i_expr.  Note that an i-expr will consume any line comments
// or hspaces before it returns. 
// Non-tokenizing implemenation notes:
// Note that i_expr will consume any line comments (line comments after
// content, as well as lines that just contain indents and comments).
// Note also that i-expr may set the the current indent to a different value
// than the indent used on entry to body; the latest indent is compared by
// the special terminals DEDENT and BADDENT.
body    :        i_expr (same body /*= (cons $i_expr $body) */
                        | dedent   /*= (list $1) */ );


restart_contents 
        : i_expr
           (comment_eol+
             (restart_contents /*= (cons $i_expr restart_contents) */
             | empty /*= (list $i_expr) */)
           | empty   /*= (list $i_expr) */)
          | indent error /*= (read_error "Bad indent inside restart list") */ ;

// Restarts.  In a non-tokenizing system, reading RESTART_END will set the current indent,
// causing dedents all the way back to here.
restart_list 
        : RESTART hspace* /*= (push_indent "") */ comment_eol*
          (restart_contents /*= $restart_contents */
          | empty /*= '() */ )
          RESTART_END hspace*;

// "i-expr" (indented sweet expressions expressions)
// is the main production for sweet-expressions in the usual case.
// This can be implemented with one-character-lookahead by also
// passing in the "current" indent ("" to start), and having it return
// the "new current indent".  The same applies to body.
// If the line after a "head" has the same or smaller indentation,
// that will end this i-expr (because it won't match INDENT),
// returning to a higher-level production.

i_expr : head (splice hspace*
                (options {greedy=true;} :
                 // TODO: Extension, allow \\ EOL to continue line.
                 // Should this be an error instead?
                 comment_eol same i_expr /*= (append $head $i_expr) */
                 // Normal case: splice ends i_expr immediately.
                 | empty /*= $head */ )
              | DOLLAR hspace*
                (i_expr /*= (list (monify $head) $i_expr) */
                | comment_eol
                  (indent body /*= (list $body) */
                  | dedent error))
              | restart_list
                (i_expr
                | comment_eol
                  (indent body
                  | empty))
              | comment_eol // Normal case, handle child lines if any:
                (indent body /*= (append $head $body) */
                | empty      /*= (monify $head) */ /* No child lines */ ))
         | (GROUP | scomment) hspace*
             (i_expr /*= $i_expr */ /* stuff afterward - ignore GROUP/scomment */
             | comment_eol
               (indent body /*= $body */  /* Normal use for GROUP */
               | same i_expr /*= $i_expr */  /* Plausible separator */
               | dedent error /*= (read_error "Dedent not allowed after group or special comment") */ ))
         | DOLLAR hspace*(i_expr /*= (list $i_expr) */ | comment_eol indent body /*= (list $body) */ )
         | restart_list (i_expr | comment_eol (indent body)?)
         | abbrevh hspace*
           (i_expr /*= (list $abbrevh $i_expr) */
           | (comment_eol
               (indent body /*= (list $abbrevh $i_expr) */ )
               | dedent error))
         ;

// Top-level sweet-expression production; handle special cases, or drop to i_expr
// in normal case.
t_expr  : comment_eol t_expr /*= $t_expr */ /* Initial lcomment, try again */
        | hspace+ (n_expr /*= $n_expr */ /* indent processing disabled */
                   | comment_eol t_expr /*= $t_expr */ /* Indented lcomment, try again */
                   | BANG error /*= (read_error "! Not allowed at top") */ )
        | BANG error /*= (read_error "! Not allowed at top") */
        | EOF /*= EOF */ /* End of file */
        | i_expr /*= $i_expr */ /* Normal case */;


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
