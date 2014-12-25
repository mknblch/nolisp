package de.mknblch.nolisp.features.macro;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"macro?"})
public class IsMacroForm implements Form {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return args.car() instanceof Macro;
    }
}    