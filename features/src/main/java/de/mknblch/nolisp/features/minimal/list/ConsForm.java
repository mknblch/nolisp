package de.mknblch.nolisp.features.minimal.list;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"cons"})
public class ConsForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object a = args.car();
        final Object b = args.cadr();
        return cons(a, b);
    }

    private static Object cons (Object a, Object b) {
        if (TypeHelper.isEmptyList(a) && TypeHelper.isEmptyList(b)) {
            return new ListStruct();
        }
        if (TypeHelper.isEmptyList(b)) {
            return new ListStruct(a);
        }
        if (TypeHelper.isList(b)) {
            if(TypeHelper.isEmptyList(a)) {
                return new ListStruct(null).setCdr((ListStruct) b);
            }
            return new ListStruct(a).setCdr((ListStruct) b);
        }
        return new ListStruct(a, b);
    }
}    