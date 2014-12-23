package de.mknblch.nolisp.features.lang;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.parser.Parser;
import de.mknblch.nolisp.generator.Define;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;

import static de.mknblch.nolisp.common.TypeHelper.asString;

/**
 * @author mknblch
 */
@Define({"load", "load-file"})
public class LoadFileSpecialForm extends BuiltInSpecialForm  {

    private static final Parser PARSER = new Parser();

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final File file = new File(TypeHelper.asString(interpreter.eval(args.car(), context)));
        return interpreter.evalEach(PARSER.parse(new BufferedInputStream(new FileInputStream(file))), context);
    }
}