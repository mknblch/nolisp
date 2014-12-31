package de.mknblch.nolisp.features.lang;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.annotations.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.parser.Parser;

import java.io.BufferedInputStream;
import java.net.URL;

/**
 * @author mknblch
 */
@Define({"load-url"})
public class LoadURLSpecialForm implements SpecialForm {

    private static final Parser PARSER = new Parser();

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final URL url = new URL(TypeHelper.asString(interpreter.eval(args.car(), context)));
        return interpreter.evalEach(PARSER.parse(new BufferedInputStream(url.openStream())), context);
    }
}