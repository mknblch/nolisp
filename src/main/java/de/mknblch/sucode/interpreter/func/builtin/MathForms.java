package de.mknblch.sucode.interpreter.func.builtin;

import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.func.Define;
import de.mknblch.sucode.parser.structs.ListStruct;

import static de.mknblch.sucode.interpreter.func.TypeHelper.asReal;

/**
 * Created by mknblch on 12.10.2014.
 */
public class MathForms {

    @Define(symbol = {"+", "add", "sum"})
    public static Object plus(ListStruct args, Context context) throws Exception {
        Object result = 0;
        for (Object arg : args) {
            final Object evaluated = Interpreter.eval(arg, context);
            result = plus(result, evaluated);
        }
        return result;
    }

    @Define(symbol = {"-", "sub"})
    public static Object minus(ListStruct args, Context context) throws Exception {
        return minus(Interpreter.eval(args.car(), context), Interpreter.eval(args.cdr().car(), context));
    }

    @Define(symbol = {"*", "mul"})
    public static Object mul(ListStruct args, Context context) throws Exception {
        return mul(Interpreter.eval(args.car(), context), Interpreter.eval(args.cdr().car(), context));
    }

    @Define(symbol = {"/", "div"})
    public static Object div(ListStruct args, Context context) throws Exception {
        return div(Interpreter.eval(args.car(), context), Interpreter.eval(args.cdr().car(), context));
    }

    @Define(symbol = {"%", "mod"})
    public static Object mod(ListStruct args, Context context) throws Exception {
        return mod(Interpreter.eval(args.car(), context), Interpreter.eval(args.cdr().car(), context));
    }

    @Define(symbol = {"**", "pow"})
    public static Object pow(ListStruct args, Context context) throws Exception {
        return Math.pow(
                asReal(Interpreter.eval(args.car(), context)),
                asReal(Interpreter.eval(args.cdr().car(), context)));
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
