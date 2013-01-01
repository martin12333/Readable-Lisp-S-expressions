
import org.antlr.runtime.*;
import scheme.*;
import static scheme.Pair.*;

public class Test {
  public static void main(String[] args) throws Exception {
    System.out.print("Starting\n");
    Pair x = cons(1, cons(2, null));
    Pair y = list("a", "b");
    Pair z = append(x, y);
    System.out.print("pair x = " + string_datum(x) + "\n");
    System.out.print("pair y = " + string_datum(y) + "\n");
    System.out.print("pair z = " + string_datum(z) + "\n");
    ANTLRInputStream input = new ANTLRInputStream(System.in);
    sweetLexer lexer = new sweetLexer(input);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    sweetParser parser = new sweetParser(tokens);
    parser.t_expr();
  }
}

