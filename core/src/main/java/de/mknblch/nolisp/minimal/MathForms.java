package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Constant;
import de.mknblch.nolisp.scanner.Define;

import java.security.SecureRandom;

/**
 * @author mknblch
 */
public class MathForms {

    @Constant("PI")
    public static final double PI = Math.PI;

    @Constant("E")
    public static final double E = Math.E;

    private static final SecureRandom SRANDOM = new SecureRandom();

    @Define({"+", "add", "sum"})
    public static Object plus(ListStruct args) throws Exception {
        Object result = 0;
        for (Object arg : args) {
            result = plus(result, arg);
        }
        return result;
    }

    @Define({"-", "sub"})
    public static Object minus(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        Object result = args.car();
        for (Object arg : args.cdr()) {
            result = minus(result, arg);
        }
        return result;
    }

    @Define({"*", "mul", "prod"})
    public static Object mul(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        Object result = args.car();
        for (Object arg : args.cdr()) {
            result = mul(result, arg);
        }
        return result;
    }

    @Define({"/", "div"})
    public static Object div(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        Object result = args.car();
        for (Object arg : args.cdr()) {
            result = div(result, arg);
        }
        return result;
    }

    @Define({"%", "mod"})
    public static Object mod(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return mod(args.car(), args.cdr().car());
    }

    @Define({"**", "pow"})
    public static Object pow(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return Math.pow(
                TypeHelper.asReal(args.car()),
                TypeHelper.asReal(args.cdr().car()));
    }

    @Define({"log"})
    public static Object log(ListStruct args) throws Exception {
        return Math.log(TypeHelper.asReal(args.car()));
    }

    @Define({"sqrt"})
    public static Object sqrt(ListStruct args) throws Exception {
        return Math.sqrt(TypeHelper.asReal(args.car()));
    }

    @Define({"log10"})
    public static Object log10(ListStruct args) throws Exception {
        return Math.log10(TypeHelper.asReal(args.car()));
    }

    @Define({"ceil"})
    public static Object ceil(ListStruct args) throws Exception {
        return Math.ceil(TypeHelper.asReal(args.car()));
    }

    @Define({"floor"})
    public static Object floor(ListStruct args) throws Exception {
        return Math.floor(TypeHelper.asReal(args.car()));
    }

    @Define({"sin"})
    public static Object sin(ListStruct args) throws Exception {
        return Math.sin(TypeHelper.asReal(args.car()));
    }

    @Define({"cos"})
    public static Object cos(ListStruct args) throws Exception {
        return Math.cos(TypeHelper.asReal(args.car()));
    }

    @Define({"tan"})
    public static Object tan(ListStruct args) throws Exception {
        return Math.tan(TypeHelper.asReal(args.car()));
    }

    @Define({"toint"})
    public static Object toInt(ListStruct args) throws Exception {
        return TypeHelper.asInt(args.car());
    }


    @Define({"toreal"})
    public static Object toReal(ListStruct args) throws Exception {
        return TypeHelper.asReal(args.car());
    }

    private static Object mod(Object a, Object b) throws EvaluationException {
        if (!(a instanceof Integer) || !(b instanceof Integer)) {
            throw new EvaluationException(
                    String.format("Expected NUMBER, NUMBER but was: %s, %s ", FormatHelper.formatAtom(a), FormatHelper.formatAtom(b)));
        }
        return (Integer) a % (Integer) b;
    }

    @Define({"rint", "random-int"})
    public static Object rint(ListStruct args) throws Exception {
        final int arint = Math.abs(SRANDOM.nextInt());
        if (null == args) return arint;
        final int max = TypeHelper.asInt(args.car());
        return max > 0 ? arint % max : 0;
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
        throw new EvaluationException(
                String.format("Expected NUMBER, NUMBER but was: %s, %s ", FormatHelper.formatAtom(a), FormatHelper.formatAtom(b)));
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
        throw new EvaluationException(
                String.format("Expected NUMBER, NUMBER but was: %s, %s ", FormatHelper.formatAtom(a), FormatHelper.formatAtom(b)));
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
        throw new EvaluationException(
                String.format("Expected NUMBER, NUMBER but was: %s, %s ", FormatHelper.formatAtom(a), FormatHelper.formatAtom(b)));
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
        throw new EvaluationException(
                String.format("Expected NUMBER, NUMBER but was: %s, %s ", FormatHelper.formatAtom(a), FormatHelper.formatAtom(b)));
    }
}
