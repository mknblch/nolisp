package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"utime"})
public class UTimeForm implements Form  {

    @Override
    public Object eval(ListStruct args) throws Exception {
        return System.currentTimeMillis();
    }
}    