package de.mknblch.nolisp.repl;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SymbolStruct;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.CoreInterpreter;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.parser.Parser;
import nolisp.Index;

import java.io.Console;

/**
 * @author mknblch
 */
public class NOLISP {


    private static Parser parser = new Parser();
    private  static final Interpreter INTERPRETER = new CoreInterpreter();

    public static void main(String[] args) throws Exception {

        if (args.length == 1 && args[0].equalsIgnoreCase("-l")) {
            repl();
        } else if(args.length == 2 && args[0].equalsIgnoreCase("-e")) {
            eval(args[1]);
        } else if(args.length == 2 && args[0].equalsIgnoreCase("-f")) {
            eval(new ListStruct(new ListStruct(new SymbolStruct("load"), args[1]))); // TODO refactor
        } else {
            System.out.println("Usage: java -jar [-e \"CODE\" | -f <FILE> | -l]");
            System.exit(1);
        }
    }

    private static void eval(String code) throws Exception {
        final ListStruct prg = parser.parse(code);
        eval(prg);
    }

    private static void eval(ListStruct prg) throws Exception {
        final Context context = makeContext();
        System.out.printf("%s", FormatHelper.formatPretty(INTERPRETER.evalEach(prg, context).last().car()));
    }

    private static Context makeContext() {
        return new Context().addDialect(Index.DIALECTS);
    }

    private static void repl() throws Exception {
        final Context context = makeContext();
        final Console console = System.console();

        while (true) {
            System.out.print("> ");
            final String line = console.readLine();
            if("exit".equals(line)) System.exit(0);
            try {
                final ListStruct prg = parser.parse(line);
                for (Object p : prg) {
                    final Object obj = INTERPRETER.eval(p, context);
                    System.out.printf("%s%n", FormatHelper.formatPretty(obj));
                }
            } catch (Exception e) {
//                System.err.printf("[%s] %s%n", e.getClass().getName(), e.getMessage());
                e.printStackTrace();
            }
        }
    }
}
