package de.mknblch.sucode.interpreter.forms.builtin;

import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.forms.Function;
import de.mknblch.sucode.parser.structs.ListStruct;

import static de.mknblch.sucode.interpreter.TypeHelper.asReal;

/**
 * Created by mknblch on 12.10.2014.
 */
public class MathForms {

    @Function(symbol = {"+", "add", "sum"})
    public static Object plus(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        Object result = 0;
        for (Object arg : args) {
            final Object evaluated = ip.eval(arg, env);
            result = plus(result, evaluated);
        }
        return result;
    }

    @Function(symbol = {"-", "sub"})
    public static Object minus(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        return minus(ip.eval(args.car(), env), ip.eval(args.cdr().car(), env));
    }

    @Function(symbol = {"*", "mul"})
    public static Object mul(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        return mul(ip.eval(args.car(), env), ip.eval(args.cdr().car(), env));
    }

    @Function(symbol = {"/", "div"})
    public static Object div(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        return div(ip.eval(args.car(), env), ip.eval(args.cdr().car(), env));
    }

    @Function(symbol = {"%", "mod"})
    public static Object mod(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        return mod(ip.eval(args.car(), env), ip.eval(args.cdr().car(), env));
    }

    @Function(symbol = {"**", "pow"})
    public static Object pow(ListStruct args, Environment env, Interpreter ip) throws EvaluationException {
        return Math.pow(
                asReal(ip.eval(args.car(), env)),
                asReal(ip.eval(args.cdr().car(), env)));
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
