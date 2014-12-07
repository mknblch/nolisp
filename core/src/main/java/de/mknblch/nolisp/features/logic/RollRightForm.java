package de.mknblch.nolisp.features.logic;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.forms.BuiltInForm;

import static de.mknblch.nolisp.common.TypeHelper.asInt;

/**
 * @author mknblch
 */
public class RollRightForm extends BuiltInForm {

    @Override
    public String[] getSymbols() {
        return new String[]{">>>"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asInt(args.car()) >>> asInt(args.cadr()) ;
    }
}    