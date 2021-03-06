package de.mknblch.nolisp.features.minimal.array;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

/**
 * @author mknblch
 */
@Define({"array-set", "aset"})
public class ArraySetForm implements Form {

    @Override
    public Object eval(ListStruct args) throws Exception {
        final Object[] objects = TypeHelper.asArray(args.car());
        ListStruct temp = args.cdr();
        while (null != temp) {
            final int index = TypeHelper.asInt(temp.car());
            Expectations.expectCdr(temp);
            objects[ index ] = temp.cadr();
            temp = temp.cddr();
        }
        return objects;
    }
}
