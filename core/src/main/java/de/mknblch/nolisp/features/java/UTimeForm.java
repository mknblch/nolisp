package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInForm;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"utime"})
public class UTimeForm extends BuiltInForm  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return System.currentTimeMillis();
    }
}    