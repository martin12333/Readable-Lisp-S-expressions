package scheme;

// (C) 2013 David A. Wheeler, released under MIT license.
// Provide a few traditional Lisp-like functions in Java,
// just barely enough functionality for sweet.g, but
// accurate for what it does.

public class Pair extends Object {

  private Object car_value, cdr_value;

  public Pair(Object x, Object y) { 
    this.car_value = x; this.cdr_value = y; 
  }


  public static Pair cons(Object x, Object y) {
    return new Pair(x, y);
  }

  public static Object car(Pair x) {
    return x.car_value;
  }

  public static Object cdr(Pair x) {
    return x.cdr_value;
  }

  public static Object cadr(Pair x) {
    return car( (Pair) cdr(x));
  }

  public static Object cddr(Pair x) {
    return cdr( (Pair) cdr(x));
  }

  public static Boolean nullp(Object x) { // Scheme "null?"
    return x == null;
  }

  public static Boolean pairp(Object x) { // Scheme "pair?"
    return x instanceof Pair;
  }

  public static Boolean equalp(Object x, Object y) { // Scheme "equal?"
    if ((x == null) && (y == null)) {
      return true;
    } else if ((x == null) || (y == null)) {
      return false ;
    } else if (pairp(x) && pairp(y)) {
      Pair px = (Pair) x;
      Pair py = (Pair) y;
      return equalp(car(px), car(py)) && equalp(cdr(px), cdr(py));
    } else if (pairp(x) || pairp(y)) {
      return false;
    } else {
      return x.equals(y);
    }
  }

  // TODO: Handle String, Int, etc. correctly.
  public static Object eqp(Object x, Object y) { // Scheme "eq?"
    return x == y;
  }

  // This does not check for cycles
  private static Boolean ends_in_null(Pair x) {
    if (nullp(cdr(x))) {
      return true;
    } else if (! pairp(cdr(x))) {
      return false;
    } else {
      return ends_in_null( (Pair) cdr(x));
    }
  }

  public static Boolean listp(Object x) { // Scheme "list?"
    if (nullp(x)) return false;
    else if (! pairp(x)) return false;
    else return ends_in_null( (Pair) x);
  }

  public static Pair list(Object x) {
    return cons(x, null);
  }

  public static Pair list(Object x, Object y) {
    return cons(x, list(y));
  }

  // "y" really needs to be a Pair, but that makes it harder to
  // to work with ANTLR
  public static Object append(Object x, Object y) {
    if (x == null) {
      return y;
    } else if (x instanceof Pair) {
      Pair p = (Pair) x;
      return cons(car(p), append(cdr(p), y));
    } else {
      throw new Error(); // for now.
    }
  }

  private static String string_datum_tail(Object x) {
    if (x == null) {
      return ")";
    } else if (x instanceof Pair) {
      Pair p = (Pair) x;
      return " " + string_datum(car(p)) + string_datum_tail(cdr(p));
    } else
      return " . " + string_datum(x) + ")";
  }

  public static String string_datum(Object x) {
    if (x == null) {
      return "()";
    } else if (x instanceof Pair) {
      Pair p = (Pair) x;
      return "(" + string_datum(car(p)) + string_datum_tail(cdr(p));
    } else {
      return x.toString();
    }
  }

// (define (mklist x)
//   (cond
//     ((not (pair? x)) (list x))
//     ((not (list? x)) (list x))
//     (#t x)))

  public static Object mklist(Object x) {
    if (! pairp(x)) {
      return list(x);
    } else if (! listp(x)) {
      return list(x);
    } else {return x;}
  }

}

