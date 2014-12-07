package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
public class ArrayInitForm extends BuiltInForm{
    @Override
    public String[] getSymbols() {
        return new String[]{"array-init", "ainit"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return new Object[asInt(args.car())];
    }
}
