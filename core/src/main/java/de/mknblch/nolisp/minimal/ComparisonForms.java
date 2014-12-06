package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Define;

/**
 * @author mknblch
 */
public class ComparisonForms {

    @Define("==")
    public static Object equal(ListStruct args) throws Exception {
        return equal(args.car(), args.cadr());
    }

    @Define("!=")
    public static Object notEqual(ListStruct args) throws Exception {
        return !equal(args.car(), args.cadr());
    }

    @Define(">")
    public static Object greater(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return greater(args.car(), args.cadr());
    }

    @Define(">=")
    public static Object greaterEqual(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return greaterEqual(args.car(), args.cadr());
    }

    @Define("<")
    public static Object lower(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return lower(args.car(), args.cadr());
    }

    @Define("<=")
    public static Object lowerEqual(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        return lowerEqual(args.car(), args.cadr());
    }

    private static boolean greater(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a > (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a > (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a > (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a > (Integer) b;
            }
        }
        throw new EvaluationException("Invalid comparison arguments.");
    }

    private static boolean equal(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return ((Double) a).compareTo((Double) b) == 0;
            }
            if (b instanceof Integer) {
                return ((Double) a).compareTo(((Integer) b).doubleValue()) == 0;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return ((Double) b).compareTo(((Integer) a).doubleValue()) == 0;
            }
            if (b instanceof Integer) {
                return ((Integer) a).compareTo((Integer) b) == 0;
            }
        }
        throw new EvaluationException("Invalid comparison arguments.");
    }

    private static boolean greaterEqual(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a >= (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a >= (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a >= (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a >= (Integer) b;
            }
        }
        throw new EvaluationException("Invalid comparison arguments.");
    }

    private static boolean lower(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a < (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a < (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a < (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a < (Integer) b;
            }
        }
        throw new EvaluationException("Invalid comparison arguments.");
    }

    private static boolean lowerEqual(Object a, Object b) throws EvaluationException {
        if (a instanceof Double) {
            if (b instanceof Double) {
                return (Double) a <= (Double) b;
            }
            if (b instanceof Integer) {
                return (Double) a <= (Integer) b;
            }
        }
        if (a instanceof Integer) {
            if (b instanceof Double) {
                return (Integer) a <= (Double) b;
            }
            if (b instanceof Integer) {
                return (Integer) a <= (Integer) b;
            }
        }
        throw new EvaluationException("Invalid comparison arguments.");
    }
}