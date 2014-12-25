package de.mknblch.nolisp.features.number;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;
import de.mknblch.nolisp.interpreter.EvaluationException;

/**
 * @author mknblch
 */
@Define({">"})
public class NumberGreaterForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return greater(args.car(), args.cadr());
    }

    private static boolean greater(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a > (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a > (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a > (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a > (Integer) b;
            }
        }
        throw new EvaluationException("Invalid comparison arguments.");
    }
}    