package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.isList;

/**
 * @author mknblch
 */
@Define({"append"})
public class AppendForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        final ListStruct ret = new ListStruct();
        for (Object arg : args) {
            if (null == arg) continue;
            if(TypeHelper.isList(arg)) {
                final ListStruct listStruct = (ListStruct) arg;
                for (Object iArg : listStruct) {
                    ret.add(iArg);
                }
            } else {
                ret.add(arg);
            }
        }
        return ret;
    }
}    