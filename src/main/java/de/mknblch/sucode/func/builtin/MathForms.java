package de.mknblch.sucode.func.builtin;

import de.mknblch.sucode.func.InjectInterpreter;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.structs.ListStruct;

import static de.mknblch.sucode.func.TypeHelper.asReal;

/**
 * Created by mknblch on 12.10.2014.
 */
public class MathForms {

    @Define(symbol = {"+", "add", "sum"})
    public static Object plus(ListStruct args, Context context) throws Exception {
        Object result = 0;
        for (Object arg : args) {
            final Object evaluated = arg; //arg;
            result = plus(result, evaluated);
        }
        return result;
    }

    @Define(symbol = {"-", "sub"})
    public static Object minus(ListStruct args, Context context) throws Exception {
        return minus(args.car(), args.cdr().car());
    }

    @Define(symbol = {"*", "mul"})
    public static Object mul(ListStruct args, Context context) throws Exception {
        return mul(args.car(), args.cdr().car());
    }

    @Define(symbol = {"/", "div"})
    public static Object div(ListStruct args, Context context) throws Exception {
        return div(args.car(), args.cdr().car());
    }

    @Define(symbol = {"%", "mod"})
    public static Object mod(ListStruct args, Context context) throws Exception {
        return mod(args.car(), args.cdr().car());
    }

    @Define(symbol = {"**", "pow"})
    public static Object pow(ListStruct args, Context context) throws Exception {
        return Math.pow(
                asReal(args.car()),
                asReal(args.cdr().car()));
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
