package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

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