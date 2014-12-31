package de.mknblch.nolisp.features.minimal.basic;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"eq?", "equal?"})
public class IsEqualForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return equal(args.car(), args.cadr());
    }

    private static Object equal(Object a, Object b) {
        if (null == a && null == b) return true;
        return null != a && null != b && a.equals(b);
    }
}    