package de.mknblch.nolisp.minilisp.launcher;

import de.mknblch.nolisp.annotations.FunctionDefinitionException;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.helper.FormatHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.CoreInterpreter;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.minilisp.MinimalLisp;
import de.mknblch.nolisp.parser.Parser;
import de.mknblch.nolisp.parser.ParserException;
import de.mknblch.nolisp.parser.lexer.LexerException;

import java.io.Console;

/**
 * @author mknblch
 */
public class NOLISP {

    private static Interpreter interpreter = new CoreInterpreter();
    private static Parser parser = new Parser();

    public static void main(String[] args) throws Exception {

        if (args.length == 1 && args[0].equalsIgnoreCase("-l")) {
            repl();
        } else if(args.length == 2 && args[0].equalsIgnoreCase("-e")) {
            eval(args[1]);
        } else {
            System.out.println("Usage: java -jar [-e CODE | -l]");
            System.exit(1);
        }


    }

    private static void eval(String code) throws Exception {
        Context context = makeContext();
        final ListStruct prg = parser.parse(code);
        System.out.printf(">%s%n", FormatHelper.formatPretty(interpreter.eval(prg, context)));
    }

    private static Context makeContext() throws FunctionDefinitionException {
        return new Context(new MinimalLisp());
    }

    private static void repl() throws FunctionDefinitionException, LexerException, ParserException {
        final Context context = makeContext();
        final Console console = System.console();

        while (true) {
            System.out.print("> ");
            final String line = console.readLine();
            if("exit".equals(line)) System.exit(0);
            final ListStruct prg = parser.parse(line);
            try {
                for (Object p : prg) {
                    final Object obj = interpreter.eval(p, context);
                    System.out.printf("%s%n", FormatHelper.formatPretty(obj));
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }



    }
}