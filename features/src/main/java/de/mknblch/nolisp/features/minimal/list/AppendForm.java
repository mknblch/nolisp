package de.mknblch.nolisp.features.minimal.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

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