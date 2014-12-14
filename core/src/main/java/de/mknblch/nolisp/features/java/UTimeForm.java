package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.scanner.Define;

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