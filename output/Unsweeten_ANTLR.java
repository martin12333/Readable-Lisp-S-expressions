
import org.antlr.runtime.*;
import scheme.*;
import static scheme.Pair.*;

// ANTLR tokenizes all at once, so don't use interactively;
// redirect a file to it.

public class Test {
  public static void main(String[] args) throws Exception {
    // System.out.print("Starting\n");
    Object x = cons(1, cons(2, null));
    Object y = list("a", "b");
    Object z = append(x, y);
    // System.out.print("pair x = " + string_datum(x) + "\n");
    // System.out.print("pair y = " + string_datum(y) + "\n");
    // System.out.print("pair z = " + string_datum(z) + "\n");
    // System.out.print("DEBUG 1:\n");
    ANTLRInputStream input = new ANTLRInputStream(System.in);
    // System.out.print("DEBUG 2:\n");
    sweetLexer lexer = new sweetLexer(input);
    // System.out.print("DEBUG 3:\n");
    CommonTokenStream tokens = new CommonTokenStream(lexer);

    // System.out.print("DEBUG 4:\n");
    sweetParser parser = new sweetParser(tokens);
    System.out.print("About to parse:\n");
    // parser.t_expr();
    while (true) {
      parser.print_t_expr();
    }
    // System.out.print("\n");
    // System.out.print("DEBUG 5\n");
    // System.out.print("\n");
  }
}

