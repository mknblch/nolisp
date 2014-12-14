package de.mknblch.nolisp.features.minimal.math;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.dialect.Define;

/**
 * @author mknblch
 */
@Define({"-", "sub"})
public class SubForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        Object result = args.car();
        for (Object arg : args.cdr()) {
            result = minus(result, arg);
        }
        return result;
    }

    private static Object minus(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a - (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a - (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a - (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a - (Integer) b;
            }
        }
        throw new EvaluationException(
                String.format("Expected NUMBER, NUMBER but was: %s, %s ", FormatHelper.formatAtom(a), FormatHelper.formatAtom(b)));
    }
}    