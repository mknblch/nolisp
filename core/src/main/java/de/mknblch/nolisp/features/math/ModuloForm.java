package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;
import de.mknblch.nolisp.interpreter.EvaluationException;

/**
 * @author mknblch
 */
public class ModuloForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"%", "modulo"};
    }

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