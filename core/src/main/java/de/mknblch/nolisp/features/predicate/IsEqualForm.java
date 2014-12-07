package de.mknblch.nolisp.features.predicate;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

/**
 * @author mknblch
 */
public class IsEqualForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{"eq?", "equal?"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return equal(args.car(), args.cadr());
    }

    private static Object equal(Object a, Object b) {
        if (null == a && null == b) return true;
        return null != a && null != b && a.equals(b);
    }
}    