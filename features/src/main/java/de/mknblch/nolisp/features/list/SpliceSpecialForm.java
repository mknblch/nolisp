package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"splice"})
public class SpliceSpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        throw new EvaluationException("Dynamic splicing not supported.");
    }
}