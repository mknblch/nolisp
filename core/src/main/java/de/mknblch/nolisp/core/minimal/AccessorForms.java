package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Converter;
import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

/**
 * @author mknblch
 */
public class AccessorForms {

    @Define("car")
    public static Object car(Context context, ListStruct args) throws Exception {
        return Converter.asList(args.car()).car();
    }

    @Define("cdr")
    public static Object cdr(Context context, ListStruct args) throws Exception {
        return Converter.asList(args.car()).car();
    }

}
