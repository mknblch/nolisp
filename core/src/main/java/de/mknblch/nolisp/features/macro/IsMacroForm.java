package de.mknblch.nolisp.features.macro;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"macro?"})
public class IsMacroForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return TypeHelper.isMacro(args.car());
    }
}    