
import org.antlr.runtime.*;
import scheme.*;
import static scheme.Pair.*;

// ANTLR tokenizes all at once, so don't use interactively;
// redirect a file to it.

public class Unsweeten_ANTLR {
  public static void main(String[] args) throws Exception {
    ANTLRInputStream input = new ANTLRInputStream(System.in);
    sweetLexer lexer = new sweetLexer(input);
    CommonTokenStream tokens = new CommonTokenStream(lexer);

    sweetParser parser = new sweetParser(tokens);
    while (true) {
      parser.start();
    }
  }
}

