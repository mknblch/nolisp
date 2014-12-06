package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.scanner.Define;

import static de.mknblch.nolisp.common.TypeHelper.asInt;

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

    @Define("<<")
    public static Object shiftLeft(ListStruct args) throws EvaluationException {
        return asInt(args.car()) << asInt(args.cadr()) ;
    }

    @Define(">>")
    public static Object shiftRight(ListStruct args) throws EvaluationException {
        return asInt(args.car()) >> asInt(args.cadr()) ;
    }

    @Define(">>>")
    public static Object rollRight(ListStruct args) throws EvaluationException {
        return asInt(args.car()) >>> asInt(args.cadr()) ;
    }

    @Define("ior")
    public static Object intOr(ListStruct args) throws EvaluationException {
        int ret = 0;
        for (Object arg : args) {
            ret = ret | asInt(arg);
        }
        return ret;
    }

    @Define("iand")
    public static Object intAnd(ListStruct args) throws EvaluationException {
        int ret = 0xFFFFFFFF;
        for (Object arg : args) {
            ret = ret & asInt(arg);
        }
        return ret;
    }
}
