package de.mknblch.nolisp.features.basic;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.parser.Parser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;

import static de.mknblch.nolisp.common.TypeHelper.asString;

/**
 * @author mknblch
 */
public class LoadFileSpecialForm extends BuiltInSpecialForm {

    private static final Parser PARSER = new Parser();

    @Override
    public String[] getSymbols() {
        return new String[]{"load", "load-file"};
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final File file = new File(asString(interpreter.eval(args.car(), context)));
        return interpreter.evalEach(PARSER.parse(new BufferedInputStream(new FileInputStream(file))), context);
    }
}