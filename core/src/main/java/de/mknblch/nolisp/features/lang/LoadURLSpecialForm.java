package de.mknblch.nolisp.features.lang;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.parser.Parser;
import de.mknblch.nolisp.dialect.Define;

import java.io.BufferedInputStream;
import java.net.URL;

import static de.mknblch.nolisp.common.TypeHelper.asString;

/**
 * @author mknblch
 */
@Define({"load-url"})
public class LoadURLSpecialForm extends BuiltInSpecialForm  {

    private static final Parser PARSER = new Parser();

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final URL url = new URL(asString(interpreter.eval(args.car(), context)));
        return interpreter.evalEach(PARSER.parse(new BufferedInputStream(url.openStream())), context);
    }
}