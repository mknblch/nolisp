package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.scanner.Define;

import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
@Define({">>"})
public class ShiftRightForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asInt(args.car()) >> asInt(args.cadr()) ;
    }
}    