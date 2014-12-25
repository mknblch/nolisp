package de.mknblch.nolisp.features.number;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;
import de.mknblch.nolisp.interpreter.EvaluationException;

/**
 * @author mknblch
 */
@Define({"=="})
public class NumberEqualForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return equal(args.car(), args.cadr());
    }

    private static boolean equal(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return ((Double) a).compareTo((Double) b) == 0;
            }
            if (b instanceof Integer) {
                return ((Double) a).compareTo(((Integer) b).doubleValue()) == 0;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return ((Double) b).compareTo(((Integer) a).doubleValue()) == 0;
            }
            if (b instanceof Integer) {
                return ((Integer) a).compareTo((Integer) b) == 0;
            }
        }
        throw new EvaluationException("Invalid comparison arguments.");
    }
}    