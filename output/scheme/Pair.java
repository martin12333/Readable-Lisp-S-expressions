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

  public static Boolean nullp(Object x) { // Scheme "null?"
    return x == null;
  }

  public static Boolean pairp(Object x) { // Scheme "pair?"
    return x instanceof Pair;
  }

  public static Pair list(Object x) {
    return cons(x, null);
  }

  public static Pair list(Object x, Object y) {
    return cons(x, list(y));
  }

  public static Pair append(Object x, Pair y) {
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

}

