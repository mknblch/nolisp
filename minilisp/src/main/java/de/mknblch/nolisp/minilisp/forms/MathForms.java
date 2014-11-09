package de.mknblch.nolisp.minilisp.forms;

import de.mknblch.nolisp.annotations.Constant;
import de.mknblch.nolisp.annotations.Define;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.helper.Expectations;
import de.mknblch.nolisp.helper.TypeHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;

/**
 * @author mknblch
 */
public class MathForms {

    @Constant("PI")
    public static final double PI = Math.PI;

    @Constant("E")
    public static final double E = Math.E;

    @Define({"+", "add", "sum"})
    public static Object plus(Context context, ListStruct args) throws Exception {
        Object result = 0;
        for (Object arg : args) {
            result = plus(result, arg);
        }
        return result;
    }

    @Define({"-", "sub"})
    public static Object minus(Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return minus(args.car(), args.cdr().car());
    }

    @Define({"*", "mul"})
    public static Object mul(Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return mul(args.car(), args.cdr().car());
    }

    @Define({"/", "div"})
    public static Object div(Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return div(args.car(), args.cdr().car());
    }

    @Define({"%", "mod"})
    public static Object mod(Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return mod(args.car(), args.cdr().car());
    }

    @Define({"**", "pow"})
    public static Object pow(Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return Math.pow(
                TypeHelper.asReal(args.car()),
                TypeHelper.asReal(args.cdr().car()));
    }

    @Define({"log"})
    public static Object log(Context context, ListStruct args) throws Exception {
        return Math.log(TypeHelper.asReal(args.car()));
    }

    @Define({"log10"})
    public static Object log10(Context context, ListStruct args) throws Exception {
        return Math.log10(TypeHelper.asReal(args.car()));
    }

    @Define({"ceil"})
    public static Object ceil(Context context, ListStruct args) throws Exception {
        return Math.ceil(TypeHelper.asReal(args.car()));
    }

    @Define({"floor"})
    public static Object floor(Context context, ListStruct args) throws Exception {
        return Math.floor(TypeHelper.asReal(args.car()));
    }

    @Define({"sin"})
    public static Object sin(Context context, ListStruct args) throws Exception {
        return Math.sin(TypeHelper.asReal(args.car()));
    }

    @Define({"cos"})
    public static Object cos(Context context, ListStruct args) throws Exception {
        return Math.cos(TypeHelper.asReal(args.car()));
    }

    @Define({"tan"})
    public static Object tan(Context context, ListStruct args) throws Exception {
        return Math.tan(TypeHelper.asReal(args.car()));
    }

    private static Object mod(Object a, Object b) throws EvaluationException {
        if (!(a instanceof Integer) || !(b instanceof Integer)) {
            throw new EvaluationException("Invalid type cast.");
        }
        return (Integer) a % (Integer) b;
    }

    private static Object plus(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a + (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a + (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a + (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a + (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }

    private static Object minus(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a - (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a - (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a - (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a - (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }

    private static Object mul(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a * (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a * (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a * (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a * (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }

    private static Object div(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a / (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a / (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a / (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a / (Integer) b;
            }
        }
        throw new EvaluationException("Invalid type cast.");
    }
}
