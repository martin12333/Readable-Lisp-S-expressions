// BNF grammar for sweet-expressions
// (c) 2012-2013 David A. Wheeler, released under "MIT" license.

// This BNF is an LL(1) grammar, written using ANTLR version 3.
// More info on ANTLR is at "http://www.antlr.org/".
// It is written to be easy to translate to a recursive-descent parser,
// even one that doesn't do full tokenizing.

// The actions are written inside {...} using Java syntax, but invoke
// normal Scheme procedures (e.g., "car" and "cons") as follows:
// +------------------|------------|------------------------------------------+
// | Action Construct | Scheme     | Discussion                               |
// |------------------|------------|------------------------------------------|
// | cons(x, y)       | (cons x y) | Java syntax used for procedure calls     |
// | nullp(x)         | (null? x)  | "?" in Scheme procedure name becomes "p" |
// | null             | '()        | Java null represents the empty list      |
// | "somename"       | 'somename  | Java strings represent atoms             |
// +------------------|------------|------------------------------------------+
// Scheme code is described directly as /*= ...Scheme code... */.

// This BNF specifically shows how non-indent whitespace is handled
// (SRFI-49's BNF is simpler but isn't specific about this, making it
// harder to interpret). Note that this BNF presumes there's a preprocessor
// that does indent processing. While indent processing is enabled, indents
// are marked with INDENT and dedents with DEDENT (one for each dedent).
// Lines with ONLY indent characters are considered blank lines.
// There is no "SAME" token to show "same indent level", but some rules
// use an empty "same" nonterminal to emphasize their lack of INDENT/DEDENT.
// Bad indentation emits BADDENT, which is never accepted.
// If there's an indent when there is NO preceding un-indented line, the
// preprocessor emits INITIAL_INDENT_NO_BANG (if no "!") or
// INITIAL_INDENT_WITH_BANG (if "!" is in the indentation); this
// does *NOT* change the indent level (this is so initial indents
// will disable indentation processing, but only on that line).

// TODO:
// - See specific TODOs below.
// - (Maybe) Define n-expr, etc.
// - (Maybe) Handle EOF in weird places.

grammar sweet;

options { k = 1; } // Force grammar to be LL(1).

// LEXER SECTION

tokens {
  BADDENT;  // Generated by indent processor, always illegal.
}

// ANTLR v3 does not allow the parser to communicate to the lexer
// (the lexer runs completely before the parser begins).
// The lexer *can* store and use state, which we have to do anyway to
// do indentation processing if we're going to do indentation processing
// inside the lexer.  Thus, in this implementation, the lexer
// tracks the parentheses enclosure level, so that \n is just a delimiter
// inside (...) but is handled specially in indentation processing,
// while symbols like "$" are just ordinary atoms inside (...).
// The lexer must also specially note "<*" and "*>" and specially modify
// the indentation levels when those are received.
// This ANTLR lexer does indentation processing, so it generates
// INDENT, DEDENT, and so on as appropriate.

@lexer::header {
  import java.util.Deque;
}
  
@lexer::members {
  // Track enclosure level. "(" etc. adds 1, ")" etc. subtracts:
  long enclosure = 0;
  Boolean initial_indent = false;
  // Are we doing indent processing?:
  private Boolean indent_processing() {
    return (enclosure == 0) && !initial_indent;
  }

  // Permit sending multiple tokens per rule, per ANTLR FAQ.
  // This is necessary to support DEDENT, as a single token may cause
  // the generation of multiple DEDENTs.
  Deque<Token> tokens = new java.util.ArrayDeque<Token>();     
  @Override
  public void emit(Token token) {
      state.token = token;
      tokens.addLast(token);
  }
  @Override
  public Token nextToken() {
      super.nextToken();
      if (tokens.isEmpty())
          return Token.EOF_TOKEN;
      return tokens.removeFirst();
  }

  // This stack records the string indents; use push, pop, peek.
  Deque<String> indents = new java.util.ArrayDeque<String>();

  private Boolean indentation_greater_than(String indent1, String indent2) {
    int len1 = indent1.length();
    int len2 = indent2.length();
    return (len1 > len2) && indent2.equals(indent1.substring(0, len2));
  }

  private void process_indent(String indent_text, Token t) {
    if (indents.isEmpty()) indents.push("");
    if (indents.peek().equals(indent_text)) { // no-op
    } else if (indentation_greater_than(indent_text, indents.peek())) {
      // System.out.print("Generate INDENT\n");
      t.setType(INDENT);
      emit(t);
      indents.push(indent_text);
    } else if (indentation_greater_than(indents.peek(), indent_text)) {
      String old_indent = "";
      while (!indents.isEmpty() &&
             indentation_greater_than(indents.peek(), indent_text)) {
        indents.removeFirst(); // drop
        // System.out.print("Generate DEDENT(s)\n");
        t.setType(DEDENT);
        emit(t);
      }
      if (!indents.peek().equals(indent_text)) { 
       // TODO: Generate System.out.print("Generate BADDENT(s)\n");
        t.setType(BADDENT);
        emit(t);
      }
    } else {
      // System.out.print("Generate BADDENT\n");
      t.setType(BADDENT);
      emit(t);
    }
  }

  private void process_initial_indent(String indent_text, Token t) {
    if (indent_text.contains("!")) {
      t.setType(INITIAL_INDENT_WITH_BANG);
    } else {
      t.setType(INITIAL_INDENT_NO_BANG);
    }
    emit(t);
    initial_indent = true;
  }
}


@parser::header {
  import scheme.*;
  import static scheme.Pair.*;
}

@parser::members {
  private void generate_eof() {
    System.exit(0); 
  }

  // ; Utility function:
  // ; If x is a 1-element list, return (car x), else return x
  // (define (monify x)
  //   (cond
  //     ((not (pair? x)) x)
  //     ((null? (cdr x)) (car x))
  //     (#t x)))

  public static Object monify(Object x) {
    if (! pairp(x)) {
      return x;
    } else if (nullp(cdr( (Pair) x))) {
      return car( (Pair) x);
    } else {return x;}
  }


  // ; --------------------------------------------------------
  // ; Curly-infix support procedures - converted from SRFI-105
  // ; --------------------------------------------------------

  // ; Return true if lyst has an even # of parameters, and the (alternating)
  // ; first parameters are "op".  Used to determine if a longer lyst is infix.
  // ; If passed empty list, returns true (so recursion works correctly).
  // (define (even-and-op-prefix? op lyst)
  //   (cond
  //     ((null? lyst) #t)
  //     ((not (pair? lyst)) #f)
  //     ((not (equal? op (car lyst))) #f) ; fail - operators not the same
  //     ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
  //     (#t   (even-and-op-prefix? op (cddr lyst))))) ; recurse.
  public static Boolean even_and_op_prefixp(Object op, Object lyst) {
    if (nullp(lyst)) {
      return true;
    } else if (! pairp(lyst)) {
      return false;
    } else if (! equalp(op, car( (Pair) lyst))) {
      return false;
    } else if (! pairp( cdr( (Pair) lyst))) {
      return false;
    } else {
      return even_and_op_prefixp(op, cddr( (Pair) lyst));
    }
  }

  // ; Return true if the lyst is in simple infix format
  // ; (and thus should be reordered at read time).
  // (define (simple-infix-list? lyst)
  //   (and
  //     (pair? lyst)           ; Must have list;  '() doesn't count.
  //     (pair? (cdr lyst))     ; Must have a second argument.
  //     (pair? (cddr lyst))    ; Must have a third argument (we check it
  //                            ; this way for performance)
  //     (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest simple
  public static Boolean simple_infix_listp(Object lyst) {
    return pairp(lyst)
           && pairp(cdr((Pair) lyst))
           && pairp(cddr((Pair) lyst))
           && even_and_op_prefixp(cadr((Pair) lyst), cdr((Pair) lyst));
  }

  // ; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  // (define (alternating-parameters lyst)
  //   (if (or (null? lyst) (null? (cdr lyst)))
  //     lyst
  //     (cons (car lyst) (alternating-parameters (cddr lyst)))))
  public static Pair alternating_parameters(Pair lyst) {
    if (nullp(lyst) || nullp(cdr(lyst))) {
      return lyst;
    } else {
      return cons(car(lyst), alternating_parameters((Pair) cddr(lyst)));
    }
  }

  // ; Not a simple infix list - transform it.  Written as a separate procedure
  // ; so that future experiments or SRFIs can easily replace just this piece.
  // Handle dollar sign specially for ANTLR.
  public static Pair transform_mixed_infix(Pair lyst) {
    return cons("$" + "nfx" + "$", lyst);
  }

  // ; Given curly-infix lyst, map it to its final internal format.
  // (define (process-curly lyst)
  //   (cond
  //    ((not (pair? lyst)) lyst) ; E.G., map {} to ().
  //    ((null? (cdr lyst)) ; Map {a} to a.
  //      (car lyst))
  //    ((and (pair? (cdr lyst)) (null? (cddr lyst))) ; Map {a b} to (a b).
  //      lyst)
  //    ((simple-infix-list? lyst) ; Map {a OP b [OP c...]} to (OP a b [c...])
  //      (cons (cadr lyst) (alternating-parameters lyst)))
  //    (#t  (transform-mixed-infix lyst))))
  public static Object process_curly(Object lyst) {
    if (! pairp(lyst)) {
      return lyst;
    } else {
      Pair plyst = (Pair) lyst;
      if (nullp(cdr(plyst))) {
        return car(plyst); // Map {a} to a.
      } else if (pairp(cdr(plyst)) && nullp(cddr(plyst))) {
        return plyst; // Map {a b} to (a b).
      } else if (simple_infix_listp(plyst)) {
        return cons(cadr(plyst), alternating_parameters(plyst));
      } else {
        return transform_mixed_infix(plyst);
      }
    }
  }
}


// Define start symbol for parser (rest of parser is later).
start : print_t_expr;


// LEXER SECTION.
// Lexical token (terminal) names are in all upper case

// These are reported as fragments, not tokens, to silence
// spurious warnings:
fragment INITIAL_INDENT_NO_BANG: ' ';
fragment INITIAL_INDENT_WITH_BANG: ' ';
fragment INDENT: ' ';
fragment DEDENT: ' ';

// Here are special interpretation for certain sequences.
// Define these first, to give them higher lexical precedence
// than other definitions.  Note that these only have special meaning
// outside (), [], and {}; otherwise they're just atoms.
GROUP_SPLICE : {indent_processing()}? => '\\' '\\'; // GROUP/splice symbol.
SUBLIST : {indent_processing()}? =>'$';
RESERVED_TRIPLE_DOLLAR : {indent_processing()}? => '$$$';  // Reserved.
RESTART : {indent_processing()}? => '<' '*' {/* TODO: Restart indent level */};
RESTART_END: {indent_processing()}? => '*' '>' {/* TODO: Restore indent level */};

// Abbreviations followed by horizontal space (space or tab) are special:
APOSH           : {indent_processing()}? => '\'' (' ' | '\t') ;
QUASIQUOTEH     : {indent_processing()}? => '\`' (' ' | '\t') ;
UNQUOTE_SPLICEH : {indent_processing()}? => ',' '@' (' ' | '\t') ;
UNQUOTEH        : {indent_processing()}? => ',' (' ' | '\t') ;

// Special end-of-line character definitions.
// Specially handle formfeed (\f) and vertical tab (\v), even though
// some argue against vertical tabs (http://prog21.dadgum.com/76.html):
fragment FF :    '\f'; // Formfeed 
fragment VT :   '\u000b';  // Vertical tab (\v).
fragment NEL:   '\u0085'; // Hi, IBM mainframes!
fragment EOL_CHAR : '\n' | '\r' | FF | VT | NEL; // These start EOL
fragment NOT_EOL_CHAR : (~ (EOL_CHAR));

// Various forms of comments - line comments and special comments:
LCOMMENT_LINE :  {(getCharPositionInLine() == 0)}? =>
     ';' NOT_EOL_CHAR* EOL_SEQUENCE {skip();};
LCOMMENT :       ';' NOT_EOL_CHAR* ; // Line comment - doesn't include EOL
BLOCK_COMMENT : '#|' // This is #| ... #|
      (options {greedy=false;} : (BLOCK_COMMENT | .))*
      '|#' ;
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
        '!#' ;
// These match #!fold-case, #!no-fold-case, #!sweet, and #!curly-infix.
SHARP_BANG_MARKER : '#!' ('a'..'z'|'A'..'Z'|'_')
                         ('a'..'z'|'A'..'Z'|'_'|'0'..'9'|'-')* ;

// The following implements INDENT/DEDENT tokens, semi-similar to Python in
// http://docs.python.org/2/reference/lexical_analysis.html#indentation
// Page 95 of "The Definitive ANTLR Reference" has a code outline for
// generating INDENT and DEDENT, but it is fundamentally wrong for us.
// That only acts when there is 1+ indent characters on a line, so it
// cannot implement all the DEDENTs necessary when it encounters a blank line.

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
// TODO
fragment SPECIAL_BLANK_LINE  : (' ' | '\t' | '!' )* ';' NOT_EOL_CHAR* EOL_SEQUENCE | (FF | VT)+ EOL_SEQUENCE;
// TODO: FF/VT
fragment INDENT_CHAR : (' ' | '\t' | '!');
fragment INDENT_CHARS : INDENT_CHAR*;
fragment INDENT_CHARS_PLUS : INDENT_CHAR+;

// EOL after contents - may have a following indented/dedented line.
EOL: {enclosure==0 &&
      ((getCharPositionInLine() != 0) && !initial_indent)}? =>
  e=EOL_SEQUENCE
  SPECIAL_BLANK_LINE*
  i=INDENT_CHARS  // This is the indent for the next line
  extra=EOL_SEQUENCE* // If this exists, the indents are useless.
  {
    $e.setType(EOL);
    emit($e);  // Emit the EOL token
    if ($extra != null || ($i.text).equals("<EOF>")) {
      process_indent("", $i); // Indented EOL = EOL
    } else {
      // Normal case: EOL, possibly followed by indent; process it.
      process_indent($i.text, $i);
    }
  } ;

BLANK_EOL : {enclosure==0 && (getCharPositionInLine() == 0)}? =>
  e=EOL_SEQUENCE
  {
    $e.setType(EOL);
    emit($e);
    process_indent("", $e);
  } ;


INITIAL_INDENT_EOL :  {enclosure==0 && initial_indent}? =>
  e=EOL_SEQUENCE
  {
    $e.setType(EOL);
    emit($e);
    initial_indent = false;
  } ;

// Generate special initial indents for initial indents
// not preceded by lines with contents
INITIAL_INDENT : {(enclosure == 0) && (getCharPositionInLine() == 0) }? =>
  i=INDENT_CHARS_PLUS
  {
    process_initial_indent($i.text, $i);
  } ;

BARE_OTHER_WS : {enclosure > 0}? => EOL_CHAR;

// Do not reference '\n' or '\r' inside a non-lexing rule in ANTLR.
// If you do, ANTLR will quietly create new lexical tokens for them, and
// those new tokens will interfere with EOL processing. E.G., do NOT do this:
//   eolchar : '\n' | '\r';

// Simple character or two-character sequences:
SPACE    : ' ';
TAB      : '\t';
fragment BANG : '!';
PERIOD   : '.';
LPAREN   : '(' {enclosure++;} ;
RPAREN   : ')' {if (enclosure>0) {enclosure--;};};
LBRACKET : '[' {enclosure++;};
RBRACKET : ']' {if (enclosure>0) {enclosure--;};};
LBRACE   : '{' {enclosure++;};
RBRACE   : '}' {if (enclosure>0) {enclosure--;};};
APOS           : '\'';
QUASIQUOTE     : '\`';
UNQUOTE_SPLICE : ',' '@';
UNQUOTE        : ',';

// Define ERROR this way so we don't get a spurious ANTLR warning.
fragment ERROR: ' ' ;

// Special non-terminals that act essentially as comments.
// They are used clarify the grammar meaning, as follows:
empty : ;  // Identifies an empty branch
same  : ;  // Emphasizes where neither indent nor dedent has occurred
error : ERROR;  // Specifically identifies an error branch.

// Note that errors can occur elsewhere, and an implementation
// may include an extension where an error is noted in this grammar.
// However, the error non-terminal makes it clear where an action is
// not defined, indicates where a parser might specifically check for
// errors, and also acts as a check on the grammar itself (to ensure that
// there isn't some valid interpretation for that sequence at that point).


// STUB BEGIN - remove this stub section for the SRFI, etc.; it's
// here for debugging and testing the grammar.  It's not an especially
// accurate representation of n-expressions, because it doesn't need to be.

// Note: The following maps most stuff into strings, because we don't
// need to do more than that for a translation.  The action rules can
// be changed to generate real values, not just string representations.


IDENTIFIER  :
  INITIAL SUBSEQUENT* |
  | '|' SYMBOL_ELEMENT* '|'
  | PECULIAR_IDENTIFIER ;

fragment INITIAL : LETTER | SPECIAL_INITIAL | INLINE_HEX_ESCAPE ;
fragment LETTER : 'a'..'z' | 'A'..'Z' ;
fragment SPECIAL_INITIAL : '!' | '$' | '%' | '&' | '*' | '/' | ':'
                           | '<' | '=' | '>' | '?' | '^' | '_' | '~';
fragment SUBSEQUENT : INITIAL | DIGIT | SPECIAL_SUBSEQUENT ;
fragment DIGIT : '0' .. '9' ;
fragment HEX_DIGIT : DIGIT |'a'..'f' ;
fragment EXPLICIT_SIGN : '+' | '-' ;
fragment SPECIAL_SUBSEQUENT : EXPLICIT_SIGN | '.' | '@' ;
fragment INLINE_HEX_ESCAPE : '\\' 'x' HEX_SCALAR_VALUE ';' ;
fragment HEX_SCALAR_VALUE : HEX_DIGIT+ ;
fragment PECULIAR_IDENTIFIER : EXPLICIT_SIGN
  ( /* empty */
    | SIGN_SUBSEQUENT SUBSEQUENT*
    | '.' DOT_SUBSEQUENT SUBSEQUENT* )
  | '.' DOT_SUBSEQUENT SUBSEQUENT*
  // Note: This is a bogus extension for testing purposes, to make sure that
  // a \\ is not interpreted in an initial indent:
  | '\\' '\\' ;
fragment DOT_SUBSEQUENT : SIGN_SUBSEQUENT | '.' ;
fragment SIGN_SUBSEQUENT : INITIAL | EXPLICIT_SIGN | '@' ;

// Annoyingly, SYMBOL_ELEMENT overlaps with STRING_ELEMENT in
// the R7RS Scheme draft 8 fix; this (below) fixes that:
fragment SYMBOL_ELEMENT :
    (~('|' | '\\'))
     | SPECIAL_STRING_ELEMENT
     // NOTE: Double-quote should NOT be here, already matched above.
     | '\\|' ; 

// boolean, character, character_name
BOOLEAN : '#t' | '#f' | '#true' | '#false' ;

CHARACTER : '#\\'
 ((~ ('a'..'z'))
  | ('a'..'w' | 'y' | 'z') ('a'..'z')*
  | 'x' HEX_SCALAR_VALUE ) ;
 
STRING : '\"' STRING_ELEMENT* '\"' ;

fragment STRING_ELEMENT :
  ~( '"' | '\\' ) | SPECIAL_STRING_ELEMENT ;

fragment SPECIAL_STRING_ELEMENT :
  '\\' ('a' | 'b' | 't' | 'n' | 'r' | '"' | '\\')
  // TODO: Intraline whitespace
  | INLINE_HEX_ESCAPE ;


// TODO: The following doesn't really follow the Scheme spec.
fragment EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;
FLOAT
    :   ('0'..'9')+ '.' ('0'..'9')* EXPONENT?
    |   '.' ('0'..'9')+ EXPONENT?
    |   ('0'..'9')+ EXPONENT ;
INT     :        ('0'..'9')+ ;
number : INT | FLOAT ;




// This more-complicated BNF is written so leading
// and trailing whitespace in a list is correctly ignored.
list_contents_real returns [Object v]
  : n1=n_expr
      (wspace+
        (lc=list_contents_real {$v = cons($n1.v, $lc.v);}
         | empty  {$v = list($n1.v);})
       | empty    {$v = list($n1.v);})
    | PERIOD
       (wspace+
         (n2=n_expr wspace* {$v = $n2.v;}
             (n3=n_expr error)?
          | empty {$v = list(".");})
       | empty {$v = list(".");})
    ;

list_contents returns [Object v]
  : wspace*
   (list_contents_real {$v=$list_contents_real.v;}
    | empty {$v = null;} ) ;


// The "greedy=true" option here forces n_expr_tail to look at the
// next character and forceably consume in *this* production if it's
// an opening paren, bracket, or brace.  Without this,
// ANTLR notices ambiguities and complains with reports like:
//  Decision can match input such as "LPAREN" using multiple alternatives: 1, 4
// This is because there are constructs such as:
//    #;a#|hi|#(x)
// which technically are ambiguous; do we mean "#;a" or "#;a(x)"?
// The option breaks the ambiguity.

n_expr_tail[Object prefix] returns [Object v]
    : (options {greedy=true;}
    : LPAREN c1=list_contents RPAREN r1=n_expr_tail[cons(prefix, $c1.v)]
      {$v = $r1.v;}
    | LBRACKET c2=list_contents RBRACKET
      r2=n_expr_tail[cons("$" + "bracket-apply" + "$", cons(prefix, $c2.v))]
      {$v = $r2.v;}
    | LBRACE   c3=list_contents RBRACE
      // Map f{} to (f), not (f ()). f{x} maps to (f x).
      r3=n_expr_tail[nullp($c3.v) ? list(prefix)
                                  : list(prefix, process_curly($c3.v))]
      {$v = $r3.v;}
    | empty
      {$v = prefix;}
    ) ;

vector returns [Object v]
  : '#(' list_contents ')' {$v = cons("vector", $list_contents.v); } ;

// Currently return value ignored because simple_datum ignores it.
bytevector returns [Object v]
  : '#u8(' list_contents ')' { $v = cons("bytevector", $list_contents.v); } ;

simple_datum   : BOOLEAN | number | CHARACTER | STRING | symbol | bytevector;
symbol : IDENTIFIER ;

compound_datum returns [Object v]
  : LPAREN norm=list_contents RPAREN {$v = $norm.v;}
  | LBRACKET bracketed=list_contents RBRACKET {$v = $bracketed.v;}
  | LBRACE braced=list_contents RBRACE {$v = process_curly($braced.v);}
  | vector {$v = $vector.v;}
  ;
  
n_expr_prefix returns [Object v]
  : simple_datum {$v = $simple_datum.text;}
  | compound_datum {$v = $compound_datum.v;}
  ;

n_expr_noabbrev returns [Object v]
    : n_expr_prefix
      n_expr_tail[$n_expr_prefix.v] {$v = $n_expr_tail.v;} ;

// STUB END


// Here we'll redefine indent/dedent as nonterminals, to make nicer results
// in the ANTLRWorks debugger parse tree
indent  : INDENT;
dedent  : DEDENT;

abbrevh returns [Object v]
  : APOSH           {$v = "quote";} /*= 'quote */
  | QUASIQUOTEH     {$v = "quasiquote";} /*= 'quasiquote */
  | UNQUOTE_SPLICEH {$v = "unquote-splicing";} /*= 'unquote-splicing */
  | UNQUOTEH        {$v = "unquote";} /*= 'unquote */ ;
abbrev_noh returns [Object v]
  : APOS            {$v = "quote";}
  | QUASIQUOTE      {$v = "quasiquote";}
  | UNQUOTE_SPLICE  {$v = "unquote-splicing";}
  | UNQUOTE         {$v = "unquote";};
abbrev_all returns [Object v]
  : abbrevh         {$v = $abbrevh.v;}
  | abbrev_noh      {$v = $abbrev_noh.v;} ;

// TODO: n_expr and n_expr_first need actions for abbreviations.
// n_expr is a full neoteric-expression.  Note that n_expr does *not*
// consume following horizontal space; this is important for correctly
// handling initially-indented lines with more than one n-expression.
n_expr returns [Object v]
 : abbrev_all n1=n_expr {$v = list($abbrev_all.v, $n1.v);}
 | n_expr_noabbrev      {$v = $n_expr_noabbrev.v;} ;

// n_expr_first is a neoteric-expression, but abbreviations
// cannot have an hspace afterwards (used by "head"):
n_expr_first returns [Object v]
  : abbrev_noh n1=n_expr_first {$v = list($abbrev_noh.v, $n1.v);}
  | n_expr_noabbrev            {$v = $n_expr_noabbrev.v;} ;
                                    

// Whitespace and indentation names
ichar   : SPACE | TAB | BANG ; // indent char - creates INDENT/DEDENTs
hspace  : SPACE | TAB ;        // horizontal space

wspace  : hspace | BARE_OTHER_WS; // Separators inside (...) etc.

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
// GROUP_SPLICE and scomment, and we permit empty contents (unlike "head").

restart_head_branch returns [Object v]
  : head
    (SUBLIST hspace* rb1=restart_head_branch {$v = list($head.v, $rb1.v);}
     | empty {$v = $head.v;} )
  | scomment hspace* rb2=restart_head_branch {$v = $rb2.v;}
  | SUBLIST hspace* rb3=restart_head_branch {$v = list($rb3.v);}
  | empty {$v = null;} /*= '() */  ;

restart_head_tail returns [Object v]
  : GROUP_SPLICE hspace* restart_head_branch again=restart_head_tail
    {$v = cons($restart_head_branch.v, $again.v); /* Interpret as splice */}
  | empty {$v = null;} ;

restart_head returns [Object v]: restart_head_branch restart_head_tail
              {$v = nullp($restart_head_branch.v) ?
                $restart_head_tail.v :
                cons($restart_head_branch.v, $restart_head_tail.v);}
             /*= (cons $restart_head_branch $restart_head_tail) */ ;

restart_contents returns [Object v]
  : i_expr comment_eol* again=restart_contents
      {$v = cons($i_expr.v, $again.v);}
  | empty {$v = null;} /* We hit RESTART_END */  ;

// A restart_list starts with an optional restart_head (one line),
// followed by optional restart_contents (0 or more i_expr's).
// We start with a head-like production, not an i_expr, to simplify the BNF.
// We'll consume hspace* at the end of this production; the RESTART_END
// token wouldn't be recognized unless it was delimited anyway, and
// consuming hspace* after it means we can avoid using the
// complex BNF construct "(hspace+ (x | empty) | empty)".
// RESTART and RESTART_END set/restore the indent level
// In a non-tokenizing implementation, reading RESTART_END inside i_expr
// will set the current indent, causing dedents all the way back to here.
restart_list returns [Object v]: RESTART hspace* restart_head
  (RESTART_END hspace*
      {$v = nullp($restart_head.v) ? null : list(monify($restart_head.v));}
   | comment_eol+ restart_contents
         {$v = nullp($restart_head.v) ? $restart_contents.v
                       : append(monify($restart_head.v), $restart_contents.v); }
     RESTART_END hspace* );


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

head returns [Object v]
 :  PERIOD
    (hspace+
      (n_expr1=n_expr hspace* {$v = list($n_expr1.v);} (n_expr2=n_expr error)?
       | empty  {$v = list(".");} /*= (list '.) */ )
     | empty    {$v = list(".");} /*= (list '.) */ )
 | restart_list
     (rest1=rest {$v = cons($restart_list.v, $rest1.v) ; }
      | empty    {$v = $restart_list.v; } )
 | n_expr_first (
     (hspace+
       (rest2=rest  {$v = cons($n_expr_first.v, $rest2.v);}
        | empty {$v = list($n_expr_first.v);} ))
      | empty   {$v = list($n_expr_first.v);} ) ;

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
// Note that "rest" is very similar to "head" - a recursive descent parser
// might implement "head" and "rest" as a single function with a parameter
// that says if it's the first one (head) or not.

rest returns [Object v]
  : PERIOD
      (hspace+
        (n_expr1=n_expr hspace* /* improper list. */
           {$v = $n_expr1.v;}
           (n_expr2=n_expr error)?
         | empty {$v = list(".");})
       | empty   {$v = list(".");})
  | scomment hspace* (rest1=rest {$v = $rest1.v;} | empty {$v = null;} )
  | restart_list
    (rest2=rest {$v = cons(list(monify($restart_list.v)), $rest2.v); }
     | empty {$v = list(monify($restart_list.v)); } )
  | n_expr3=n_expr
      ((hspace+ (rest3=rest {$v = cons($n_expr3.v, $rest3.v);}
                 | empty {$v = list($n_expr3.v);} ))
       | empty           {$v = list($n_expr3.v);} ) ;


// "body" handles the sequence of 1+ child lines in an i_expr
// (e.g., after a "head"), each of which is itself an i_expr.
// It returns the list of expressions in the body.
// Note that an i-expr will consume any line comments or hspaces
// before it returns back to the "body" production.
// Non-tokenizing implementation notes:
// Note that i_expr will consume any line comments (line comments after
// content, as well as lines that just contain indents and comments).
// Note also that i-expr may set the the current indent to a different value
// than the indent used on entry to body; the latest indent is compared by
// the special terminals DEDENT and BADDENT.

body returns [Object v] :
  i_expr
    (same body1=body {$v = cons($i_expr.v, $body1.v);}
     | dedent        {$v = list($i_expr.v);} ) ;

// "i-expr" (indented sweet-expressions)
// is the main production for sweet-expressions in the usual case.
// This can be implemented with one-character-lookahead by also
// passing in the "current" indent ("" to start), and having it return
// the "new current indent".  The same applies to body.
// If the line after a "head" has the same or smaller indentation,
// that will end this i-expr (because it won't match INDENT),
// returning to a higher-level production.

// SUBLIST is handled in i_expr, not in "head", because if there
// are child lines, those child lines are parameters of the right-hand-side,
// not of the whole production.

// Note: In a non-tokenizing implementation, a RESTART_END may be
// returned by head, which ends a list of i_expr inside a restart.  i_expr
// should then set the current_indent to RESTART_END, and return, to signal
// the reception of RESTART_END.

// TODO: Make "(head|empty) SUBLIST comment_eol" be an error; use instead:
// $ \\    ; Use SUBLIST GROUP, not just SUBLIST, if you want children.
//   1 2
//   3 4

i_expr returns [Object v]
  : head
    (GROUP_SPLICE hspace* /* Not initial; interpret as splice */
      (options {greedy=true;} :
        comment_eol error
          // To allow \\ EOL as line-continuation, could instead do:
          //    comment_eol same i9=i_expr {append($head.v, $i9.v);}
          // John Cowan recommends (Sat, 29 Dec 2012 13:18:18 -0500)
          // that we *not* do this, because it'd be confusing.
          // Normal case: splice ends i_expr immediately:
        | empty {$v = monify($head.v);} )
     | SUBLIST hspace* i_expr1=i_expr
       {$v=list(monify($head.v), $i_expr1.v);}
     | comment_eol // Normal case, handle child lines if any:
       (indent body2=body {$v = append($head.v, $body2.v);}
        | empty     {$v = monify($head.v);} /* No child lines */ ))
  | (GROUP_SPLICE | scomment) hspace* /* Initial; Interpet as group */
      (i_expr2=i_expr {$v = $i_expr2.v;} /* Ignore GROUP/scomment if initial */
       | comment_eol
         (indent body3=body {$v = $body3.v;} /* Normal use for GROUP */
          | same i_expr3=i_expr {$v = $i_expr3.v;} /* Plausible separator */
          | dedent error ))
  | SUBLIST hspace* i_expr4=i_expr /* "$" as first expression on line */
      {$v=list($i_expr4.v);}
  | abbrevh hspace* i_expr5=i_expr
      {$v=list($abbrevh.v, $i_expr5.v);}
  ;

// Top-level sweet-expression production, t_expr.
// This production handles special cases, then in the normal case
// drops to the i_expr production.

// The rule for "indent processing disabled on initial top-level hspace"
// is a very simple (and clever) BNF construction by Alan Manuel K. Gloria.
// If there is an indent it simply reads a single n-expression and returns.
// If there is more than one on an initially-indented line, the later
// horizontal space will not have have been read, so this production will
// fire again on the next invocation, doing the right thing.

// Although "!" is an indent character, it's an error to use it at the
// topmost level.  The only reason to indent at the top is to disable
// indent processing, for backwards compatibility.  Detecting this as
// an error should detect some mistakes.

t_expr returns [Object v]
  : comment_eol t_expr1=t_expr {$v=$t_expr1.v;} /* Initial lcomment, retry */
  | (INITIAL_INDENT_NO_BANG | hspace+ )
    (n_expr {$v = $n_expr.v;} /* indent processing disabled */
     | comment_eol t_expr2=t_expr {$v=$t_expr2.v;} )
  | INITIAL_INDENT_WITH_BANG error
  | EOF {generate_eof();} /* End of file */
  | i_expr {$v = $i_expr.v;} /* Normal case */ ;

print_t_expr:
  t_expr {System.out.print(string_datum($t_expr.v) + "\n"); } ;

