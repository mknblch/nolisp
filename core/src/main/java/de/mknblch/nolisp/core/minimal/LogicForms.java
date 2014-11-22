package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;

/**
 * @author mknblch
 */
public class LogicForms {

    @Define("and")
    public static Object and(ListStruct args) {
        for (Object arg : args) {
            if (!TypeHelper.asBoolean(arg)) return false;
        }
        return true;
    }

    @Define("or")
    public static Object or(ListStruct args) {
        for (Object arg : args) {
            if (TypeHelper.asBoolean(arg)) return true;
        }
        return false;
    }

    @Define("xor")
    public static Object xor(ListStruct args) {
        return TypeHelper.asBoolean(args.car()) ^ TypeHelper.asBoolean(args.cadr());
    }

    @Define("not")
    public static Object not(ListStruct args) {
        return !TypeHelper.asBoolean(args.car());
    }
}
