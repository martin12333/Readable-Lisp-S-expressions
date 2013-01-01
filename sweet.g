// BNF grammar for sweet-expressions
// (c) 2012 David A. Wheeler, released under "MIT" license.

// This is an LL(1) grammar, written using ANTLR version 3. More info:
// http://www.antlr.org/
// Actions are expressed as /*= ...Scheme code... */

// This BNF presumes there's a preprocessor that does
// indent processing. While indent processing is enabled, indents are
// marked with INDENT, dedents with DEDENT (one for each dedent).
// Lines with ONLY indent characters are considered blank lines.
// There is no "SAME" token to show "same indent level"; some rules include
// an empty "same" nonterminal to emphasize their lack of INDENT/DEDENT.

// TODO:
// - See specific TODOs below.
// - (Maybe) Define n-expr, etc.
// - (Maybe) Handle EOF in weird places.

// ; Utility function:
// ; If x is a 1-element list, return (car x), else return x
// (define (monify x)
//   (cond
//     ((not (pair? x)) x)
//     ((null? (cdr x)) (car x))
//     (#t x)))

grammar sweet;

options { k = 1; } // Force grammar to be LL(1).

@header {
import scheme.*;
import static scheme.Pair.*;
}

start : print_t_expr;
// start : t_expr;  // This grammar defines a sweet-expression.

// Lexer. Lexical token (terminal) names are in all upper case

// Here are special interpretation for certain sequences.
// Define these first, to give them higher lexical precedence
// than other definitions.
GROUP   :        '\\' '\\';      // GROUP and splice symbol.
DOLLAR  :       '$';             // Not an atom unless inside {}, (), or [].
RESERVED_TRIPLE_DOLLAR : '$$$';  // Reserved for future use.
RESTART :       '<' '*';
RESTART_END:    '*' '>';

// Abbreviations followed by horizontal space (space or tab) are special:
APOSH           : '\'' (' ' | '\t') ;
QUASIQUOTEH     : '\`' (' ' | '\t') ;
UNQUOTE_SPLICEH : ',' '@' (' ' | '\t') ;
UNQUOTEH        : ',' (' ' | '\t') ;

// Special end-of-line character definitions.
// Specially handle formfeed (\f) and vertical tab (\v), even though
// some argue against vertical tabs (http://prog21.dadgum.com/76.html):
fragment FF :    '\f'; // Formfeed 
fragment VT :   '\u000b';  // Vertical tab (\v).
fragment NEL:   '\u0085'; // Hi, IBM mainframes!
fragment EOL_CHAR : '\n' | '\r' | FF | VT | NEL; // These start EOL
fragment NOT_EOL_CHAR : (~ (EOL_CHAR));

// Various forms of comments - line comments and special comments:
LCOMMENT :       ';' NOT_EOL_CHAR* ; // Line comment - doesn't include EOL
BLOCK_COMMENT : '#|' // This is #| ... #|
      (options {greedy=false;} : (BLOCK_COMMENT | .))*
      '|#' {$channel=HIDDEN;} ;
DATUM_COMMENT_START : '#;' ;
// SRFI-105 notes that "implementations could trivially support
// (simultaneously) markers beginning with #! followed by a letter
// (such as the one to identify support for curly-infix-expressions),
// the SRFI-22 #!+space marker as an ignored line, and the
// format #!/ ... !# and #!. ... !# as a multi-line comment."
// We'll implement that approach for maximum flexibility.
SRFI_22_COMMENT         :       '#! ' NOT_EOL_CHAR* ;
SHARP_BANG_FILE :       '#!' ('/' | '.')
        (options {greedy=false;} : .)*
        '!#' {$channel=HIDDEN;} ;
// These match #!fold-case, #!no-fold-case, #!sweet, and #!curly-infix.
SHARP_BANG_MARKER : '#!' ('a'..'z'|'A'..'Z'|'_')
                         ('a'..'z'|'A'..'Z'|'_'|'0'..'9'|'-')* ;

// End-of-line (EOL) is extremely special in sweet-expressions.
// After reading it, we'll need to read in any following indent characters
// (if indent processing is active) to determine if have an INDENT or DEDENTs.
// As part of tokenizing, we'll consume any following lines that
// are ;-only lines, and treat indent-only lines equivalent to blank lines.
// We support lone formfeeds on a line to support the GNU Coding Standards
// (http://www.gnu.org/prep/standards/standards.html), which says:
// "Please use formfeed characters (control-L) to divide the program
// into pages at logical places (but not within a function)...
// The formfeeds should appear alone on lines by themselves."
// Thus, FF and VT are supported, but they must be at the beginning
// of a line and do not themselves create a newline (they still have
// to be followed by an EOL_SEQUENCE).
fragment EOL_SEQUENCE : ('\r' '\n'? | '\n' '\r'? | NEL);
fragment BLANK_LINE   : (' ' | '\t')* ';' NOT_EOL_CHAR* | (FF | VT)+ ;
EOL     : (FF | VT)* EOL_SEQUENCE
          (BLANK_LINE EOL_SEQUENCE)* ;

// Do not reference '\n' or '\r' inside a non-lexing rule in ANTLR.
// If you do, ANTLR will quietly create new lexical tokens for them, and
// those new tokens will interfere with EOL processing. E.G., do NOT do this:
//   eolchar : '\n' | '\r';

// Simple character or two-character sequences:
SPACE    : ' ';
TAB      : '\t';
SHARP    : '#';
BANG     : '!';
PERIOD   : '.';
LPAREN   : '(';
RPAREN   : ')';
LBRACKET : '[';
RBRACKET : ']';
LBRACE   : '{';
RBRACE   : '}';
APOS           : '\'';
QUASIQUOTE     : '\`';
UNQUOTE_SPLICE : ',' '@';
UNQUOTE        : ',';

// Special non-terminals that act essentially as comments.
// They are used clarify the grammar meaning, as follows:
empty : ;  // Identifies an empty branch
same  : ;  // Emphasizes where neither indent nor dedent has occurred
error : ;  // Specifically identifies an error branch.

// Note that errors can occur elsewhere, and an implementation
// may include an extension where an error is noted in this grammar.
// However, the error non-terminal makes it clear where an action is
// not defined, indicates where a parser might specifically check for
// errors, and also acts as a check on the grammar itself (to ensure that
// there isn't some valid interpretation for that sequence at that point).


// STUB BEGIN - remove this stub section for the SRFI, etc.; it's
// here for debugging and testing the grammar.  It's not an especially
// accurate representation of n-expressions, because it doesn't need to be.

// If you use this BNF directly, use \> for indent, \< for dedent:
INDENT : '\\>' ' '*;
DEDENT : '\\<' ' '*;

// For example, a valid input would be:
//   a b
//   \>c
//   d e
//   \<
// to represent:
// a b
//   c
//   d e
//

// The following is intentionally limited.  In particular, it doesn't include
// the characters used for INDENT/DEDENT.
NAME  : ('a'..'z'|'A'..'Z'|'_'|'\\') ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'\\')* ;
fragment EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;
FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT ;
INT     :        ('0'..'9')+ ;
fragment HEX_DIGIT : ('0'..'9'|'a'..'f'|'A'..'F') ;
fragment OCTAL_ESC
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ;
fragment UNICODE_ESC
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT ;
fragment ESC_SEQ
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   UNICODE_ESC
    |   OCTAL_ESC ;

STRING : '\"' ( ESC_SEQ | ~('\"'|'\\') ) '\"' ;
CHAR   : '#\\' ('!'..'@' | '['..'`' | '{'..'~' | ('A'..'Z' | 'a'..'z')+) ;

atom   : NAME | INT | FLOAT | STRING | CHAR ;

list_contents 
    : atom (wspace+ list_contents)? | empty ;

n_expr_tail 
    : LPAREN   list_contents RPAREN
    | LBRACE   list_contents RBRACE
    | LBRACKET list_contents RBRACKET ;

n_expr_noabbrev returns [Object v]
    : (atom {$v = $atom.text;}
       | LPAREN norm=list_contents RPAREN {$v = "(" + $norm.text + ")";}
       | LBRACE braced=list_contents RBRACE
       | LBRACKET bracketed=list_contents RBRACKET )
      (options {greedy=true;} : n_expr_tail)* ;

// STUB END


// Here we'll redefine indent/dedent as nonterminals, to make nicer results
// in the ANTLRWorks debugger parse tree
indent  : INDENT;
dedent  : DEDENT;

abbrevh : APOSH /*= 'quote */
        | QUASIQUOTEH /*= 'quasiquote */
        | UNQUOTE_SPLICEH /*= 'unquote-splicing */
        | UNQUOTEH /*= 'unquote */ ;
abbrev_noh : APOS | QUASIQUOTE | UNQUOTE_SPLICE | UNQUOTE ;
abbrev_all : abbrevh | abbrev_noh ;
splice     : GROUP;  // Use this synonym to make its purpose clearer.
sublist    : DOLLAR; // Use this synonym to make its purpose clearer.


// n_expr is a full neoteric-expression.  Note that n_expr does *not*
// consume following horizontal space; this is important for correctly
// handling initially-indented lines with more than one n-expression.
n_expr returns [Object v]: abbrev_all* n_expr_noabbrev
                              {$v = $n_expr_noabbrev.v;};

// n_expr_first is a neoteric-expression, but abbreviations
// cannot have an hspace afterwards (used by "head"):
n_expr_first returns [Object v]:    abbrev_noh* n_expr_noabbrev
                                    { System.out.print("DEBUG: n_expr_first produced " + $n_expr_noabbrev.v + "\n");
                                    $v = $n_expr_noabbrev.v;};

// Whitespace and indentation names
ichar   : SPACE | TAB | BANG ; // indent char - creates INDENT/DEDENTs
hspace  : SPACE | TAB ;        // horizontal space

wspace  : hspace;  // or eolchars

// "Special comments" (scomments) are comments other than ";" (line comments):
sharp_bang_comments : SRFI_22_COMMENT | SHARP_BANG_FILE | SHARP_BANG_MARKER ;
scomment : BLOCK_COMMENT
         | DATUM_COMMENT_START hspace* n_expr
         | sharp_bang_comments ;

// Read in ;comment (if exists), followed by EOL.  EOL consumes
// additional comment-only lines (if any).  On a non-tokenizing parser,
// this may reset indent as part of EOL processing.

comment_eol : LCOMMENT? EOL;

// This BNF uses the following slightly complicated pattern in many places:
//   from_n_expr ((hspace+ (stuff /*= val1 */ | empty /*= val2 */ ))
//                | empty                             /*= val2 */ )
// This is an expanded form of this BNF pattern (sans actions):
//   from_n_expr (hspace+ stuff?)?
// Note that this pattern quietly removes horizontal spaces at the
// end of the line correctly; that's important because you can't see them,
// so quietly handling them eliminates a source of hard-to-find and
// unnecessary errors.
// If from_n_expr (etc.) is as greedy as possible (it needs to be),
// we *could* instead accept this simpler BNF pattern:
//   from_n_expr hspace* stuff?
// but while that simpler BNF pattern would correctly accept *good* input,
// it would also accept *incorrect* input like "a(1)q" or other n-expressions
// followed immediately by other n-expressions without intervening whitespace.
// We want to detect such situations as errors, so we'll use the
// more complex (and more persnickety) BNF pattern instead.


// Here we handle restarts, which are a special case.

// Handle the first line of a restart.  This is very similar to "head";
// it reads in a line, and returns a list of its neoteric-expressions;
// it consumes all trailing hspace.
// We have to handle restart_head differently from head, because
// i_expr is designed to support child lines, but the first line can't have
// child lines.  This creates a "head-like" with functionality we CAN support.
// Thus, we call on "head" to do many things, but we specially handle leading
// GROUP and scomment, and we permit empty contents (unlike "head").
// Be greedy, because "GROUP" and "splice" are actually the same symbol;
// we need to prefer GROUP where it makes sense.
restart_head_branch returns [Object v]:
              head (DOLLAR hspace* restart_head_branch /*= (list $head $restart_head) */
                    | empty /*= $head */ )
              | scomment hspace* restart_head_branch /*= $restart_head */
              | DOLLAR hspace* restart_head_branch /*= (list $restart_head) */
              | empty /*= '() */
              ;

restart_head_tail returns [Object v]: splice hspace* restart_head_branch restart_head_tail
             /*= (cons $restart_head_branch $restart_head_tail) */
             | empty /*= '() */ ;

restart_head returns [Object v]: restart_head_branch restart_head_tail
             /*= (cons $restart_head_branch $restart_head_tail) */ ;

restart_contents returns [Object v]: i_expr comment_eol* restart_contents
               /*= (cons $i_expr $restart_contents) */
              | empty /*= '() */ /* Hit RESTART_END */
              ;

// A restart_list starts with an optional restart_head (one line),
// followed by optional restart_contents (0 or more i_expr's).
// We start with a head-like production, not an i_expr, to simplify the BNF.
// We'll consume hspace* at the end of this production; the RESTART_END
// token wouldn't be recognized unless it was delimited anyway, and
// consuming hspace* after it means we can avoid using the
// complex BNF construct "(hspace+ (x | empty) | empty)".
// In a non-tokenizing implementation, reading RESTART_END inside i_expr
// will set the current indent, causing dedents all the way back to here.
restart_list returns [Object v]: RESTART hspace* restart_head
          ( RESTART_END hspace*
            /*= (if (null? $restart_head) '() (list (monify $restart_head))) */
           | /*= (push_indent "") */
             comment_eol+
             restart_contents
                /*= (if (null? $restart_head)
                      $restart_contents
                      (cons $restart_head $restart_contents)) */
             RESTART_END /*= (restore_indent) */ hspace* );


// The "head" is the production for 1+ n-expressions on one line; it will
// return the list of n-expressions on the line.  If there is one n-expression
// on the line, it returns a list of exactly one item; this makes it
// easy to append to later (if appropriate).  In some cases, we want
// single items to be themselves, not in a list; function monify does this.
// The "head" production never reads beyond the current line
// (except within a block comment), so it doesn't need to keep track
// of indentation, and indentation will NOT change within head.
// Callers can depend on "head" and "after" *not* changing indentation.
// On entry, all indentation/hspace must have already been read.
// On return, it will have consumed all hspace (spaces and tabs).
// On a non-tokenizing recursive descent parser, the "head" and its callees
// have to also read and determine if the n-expression is special
// (e.g., //, $, #!...!#, abbreviation + hspace), and have it return a
// distinct value if it is; head and friends operate a lot like a tokenizer
// in that case.

head returns [Object v]:  PERIOD
           (hspace+
              ((n_expr hspace* /*= (list $n_expr) */ (n_expr error)?)
               | empty  /*= (list '.) */ )
            | empty     /*= (list '.) */ )
        | restart_list
          (rest1=rest /*= (cons (list (monify $restart_list)) $rest) */
           | empty /*= (list (monify $restart_list)) */ )
        | n_expr_first (
           (hspace+
             (rest2=rest    {$v = cons($n_expr_first.v, $rest2.v);} /*= (cons $n_expr_first $rest) */
              | empty {$v = list($n_expr_first.v);} /*= (list $n_expr_first) */ ))
            | empty   {$v = list($n_expr_first.v);} /*= (list $n_expr_first) */  ) ;

// The "rest" production reads the rest of the expressions on a line
// (the "rest of the head"), after the first expression of the line.
// Like head, it consumes any hspace before it returns.
// The "rest" production is written this way so a non-tokenizing
// implementation can read an expression specially. E.G., if it sees a period,
// read the expression directly and then see if it's just a period.
// Note that unlike the first head expression, block comments and
// datum comments that don't begin a line (after indent) are consumed,
// and abbreviations followed by a space merely apply to the
// next n-expression (not to the entire indented expression).

rest returns [Object v]   : PERIOD hspace+ n_expr hspace* /* improper list. */
          /*= $n_expr */  // TODO: Handle period "." end-of-line?
          (n_expr error)? /* Shouldn't have another n_expr! */
        | scomment hspace* (rest1=rest /*= $rest */ | empty /*= '() */ )
        | restart_list
            (rest2=rest /*= (cons (list (monify $restart_list)) $rest) */
             | empty /*= list (monify $restart_list)) */ )
        | n_expr
            ((hspace+ (rest3=rest {$v = cons($n_expr.v, $rest3.v);} /*= (cons $n_expr $rest) */
                       | empty {$v = list($n_expr.v);} /*= (list $n_expr) */ ))
              | empty {$v = list($n_expr.v);} /*= (list $n_expr) */) ;


// "body" handles the sequence of 1+ child lines in an i_expr
// (e.g., after a "head"), each of which is itself an i_expr.
// Note that an i-expr will consume any line comments or hspaces
// before it returns back to the "body" production.
// Non-tokenizing implemenation notes:
// Note that i_expr will consume any line comments (line comments after
// content, as well as lines that just contain indents and comments).
// Note also that i-expr may set the the current indent to a different value
// than the indent used on entry to body; the latest indent is compared by
// the special terminals DEDENT and BADDENT.

body  returns [Object v]  :        i_expr (same body1=body {$v = cons($i_expr.v, $body1.v);} /*= (cons $i_expr $body) */
                        | dedent   {$v = list($i_expr.v);} /*= (list $i_expr) */ ) ;

// "i-expr" (indented sweet-expressions)
// is the main production for sweet-expressions in the usual case.
// This can be implemented with one-character-lookahead by also
// passing in the "current" indent ("" to start), and having it return
// the "new current indent".  The same applies to body.
// If the line after a "head" has the same or smaller indentation,
// that will end this i-expr (because it won't match INDENT),
// returning to a higher-level production.

// DOLLAR is handled in i_expr, not in "head", because if there
// are child lines, those child lines are parameters of the right-hand-side,
// not of the whole production.

// Note: In a non-tokenizing implementation, a RESTART_END may be
// returned by head, which ends a list of i_expr inside a restart.  i_expr
// should then set the current_indent to RESTART_END, and return, to signal
// the reception of RESTART_END.

i_expr returns [Object v] :
         head (splice hspace*
                (options {greedy=true;} :
                 comment_eol error
                 // Could instead do:
                 //    comment_eol same i_expr /*= (append $head $i_expr) */
                 // to allow \\ EOL as line-continuation.
                 // John Cowan recommends (Sat, 29 Dec 2012 13:18:18 -0500)
                 // that we *not* do this, because it'd be confusing.
                 // Normal case: splice ends i_expr immediately:
                 | empty {$v = $head.v;} /*= $head */ )
              | DOLLAR hspace*
                (i_expr1=i_expr {$v=list(monify($head.v), $i_expr1.v);} /*{$v = list(monify($head.v), $i_expr.v);}*/ /*= (list (monify $head) $i_expr) */
                 | comment_eol indent body1=body {$v=list($body1.v);} /*= (list $body) */ )
              | comment_eol // Normal case, handle child lines if any:
                (indent body2=body {$v = append($head.v, $body2.v);} /*= (append $head $body) */
                 | empty     {$v = monify($head.v);} /*= (monify $head) */ /* No child lines */ ))
         | (GROUP | scomment) hspace*
             (i_expr2=i_expr {$v = $i_expr2.v;} /*= $i_expr */ /* ignore the GROUP/scomment */
             | comment_eol
               (indent body3=body {$v = $body3.v;} /*= $body */  /* Normal use for GROUP */
                | same i_expr3=i_expr {$v = $i_expr3.v;} /*= $i_expr */  /* Plausible separator */
                | dedent error ))
         | DOLLAR hspace* (i_expr4=i_expr  {$v=list($i_expr4.v);}  /*= (list $i_expr) */
                           | comment_eol indent body4=body {$v=list($body4.v);} /*= (list $body) */ )
         | abbrevh hspace*
           (i_expr5=i_expr /*= (list $abbrevh $i_expr) */
            | (comment_eol indent body5=body /*= (list $abbrevh $i_expr) */ ))  ;

// Top-level sweet-expression production, t_expr.
// This production handles special cases, then in the normal case
// drops to the i_expr production.

// The rule for "indent processing disabled on initial top-level hspace"
// is a very simple (and clever) BNF construction by Alan Manuel K. Gloria.
// If there is an indent it simply reads a single n-expression and returns.
// If there is more than one on an initially-indented line, the later
// horizontal space will not have have been read, so this production will
// fire again on the next invocation.

// Although "!" is an indent character, it's an error to use it at the
// topmost level.  The only reason to indent at the top is to disable
// indent processing, for backwards compatibility.  Detecting this as
// an error should detect some mistakes.

t_expr  returns [Object v]
  : comment_eol t_expr /*= $t_expr */ /* Initial lcomment, try again */
        | hspace+
          (n_expr { $v = $n_expr.v; } /* indent processing disabled. */
           | comment_eol t_expr /*= $t_expr */ /* Indented lcomment */
           | BANG error )
        | BANG error
        | EOF /*= EOF */ /* End of file */
        | i_expr {$v = $i_expr.v;} /*= $i_expr */ /* Normal case */ ;

print_t_expr
	:	t_expr {System.out.print(string_datum($t_expr.v)); } ;

