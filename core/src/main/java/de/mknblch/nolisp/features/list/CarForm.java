package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

import static de.mknblch.nolisp.common.TypeHelper.asList;

/**
 * @author mknblch
 */
@Define({"car"})
public class CarForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return asList(args.car()).car();
    }
}    