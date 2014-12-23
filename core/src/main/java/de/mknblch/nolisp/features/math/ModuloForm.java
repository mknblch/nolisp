package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"%", "modulo"})
public class ModuloForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return mod(args.car(), args.cdr().car());
    }

    private static Object mod(Object a, Object b) throws EvaluationException {
        if (!(a instanceof Integer) || !(b instanceof Integer)) {
            throw new EvaluationException(
                    String.format("Expected NUMBER, NUMBER but was: %s, %s ", FormatHelper.formatAtom(a), FormatHelper.formatAtom(b)));
        }
        return (Integer) a % (Integer) b;
    }
}    