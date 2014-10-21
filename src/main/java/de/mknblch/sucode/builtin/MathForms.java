package de.mknblch.sucode.builtin;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.ast.ListStruct;

import static de.mknblch.sucode.helper.TypeHelper.asReal;

/**
 * Created by mknblch on 12.10.2014.
 */
public class MathForms {

    @Define(symbol = {"+", "add", "sum"})
    public static Object plus(Context context, ListStruct args) throws Exception {
        Object result = 0;
        for (Object arg : args) {
            result = plus(result, arg);
        }
        return result;
    }

    @Define(symbol = {"-", "sub"})
    public static Object minus(Context context, ListStruct args) throws Exception {
        return minus(args.car(), args.cdr().car());
    }

    @Define(symbol = {"*", "mul"})
    public static Object mul(Context context, ListStruct args) throws Exception {
        return mul(args.car(), args.cdr().car());
    }

    @Define(symbol = {"/", "div"})
    public static Object div(Context context, ListStruct args) throws Exception {
        return div(args.car(), args.cdr().car());
    }

    @Define(symbol = {"%", "mod"})
    public static Object mod(Context context, ListStruct args) throws Exception {
        return mod(args.car(), args.cdr().car());
    }

    @Define(symbol = {"**", "pow"})
    public static Object pow(Context context, ListStruct args) throws Exception {
        return Math.pow(
                asReal(args.car()),
                asReal(args.cdar()));
    }

    private static Object mod(Object a, Object b) throws EvaluationException {
        if(!(a instanceof Integer) || !(b instanceof Integer)) {
            throw new EvaluationException("Invalid type cast.");
        }
        return (Integer)a % (Integer)b;
    }

    private static Object plus(Object a, Object b) throws EvaluationException {
        if(a instanceof Double) {
            if(b instanceof Double) {
                return (Double) a + (Double) b;
            }
            if(b instanceof Integer) {
                return (Double) a + (Integer) b;
            }
        }
        if(a instanceof Integer) {
            if(b instanceof Double) {
                return (Integer) a + (Double) b;
            }
            if(b instanceof Integer) {
                return (Integer) a + (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }

    private static Object minus(Object a, Object b) throws EvaluationException {
        if(a instanceof Double) {
            if(b instanceof Double) {
                return (Double) a - (Double) b;
            }
            if(b instanceof Integer) {
                return (Double) a - (Integer) b;
            }
        }
        if(a instanceof Integer) {
            if(b instanceof Double) {
                return (Integer) a - (Double) b;
            }
            if(b instanceof Integer) {
                return (Integer) a - (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }

    private static Object mul(Object a, Object b) throws EvaluationException {
        if(a instanceof Double) {
            if(b instanceof Double) {
                return (Double) a * (Double) b;
            }
            if(b instanceof Integer) {
                return (Double) a * (Integer) b;
            }
        }
        if(a instanceof Integer) {
            if(b instanceof Double) {
                return (Integer) a * (Double) b;
            }
            if(b instanceof Integer) {
                return (Integer) a * (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }

    private static Object div(Object a, Object b) throws EvaluationException {
        if(a instanceof Double) {
            if(b instanceof Double) {
                return (Double) a / (Double) b;
            }
            if(b instanceof Integer) {
                return (Double) a / (Integer) b;
            }
        }
        if(a instanceof Integer) {
            if(b instanceof Double) {
                return (Integer) a / (Double) b;
            }
            if(b instanceof Integer) {
                return (Integer) a / (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }
}
