package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.scanner.Define;
import de.mknblch.nolisp.scanner.Special;

import static de.mknblch.nolisp.common.TypeHelper.asInt;
import static de.mknblch.nolisp.common.TypeHelper.asList;
import static de.mknblch.nolisp.common.TypeHelper.isList;

/**
 * @author mknblch
 */
public class ListForms {

    @Special
    @Define("quote")
    public static Object quote(Interpreter interpreter, Context context, ListStruct args) {
        return args.car();
    }

    @Define({"llength", "list-length"})
    public static Object length(ListStruct args) throws EvaluationException {
        if (null == args) return 0;
        return asList(args.car()).size();
    }

    @Define("list")
    public static Object list(ListStruct args) {
        return args;
    }

    @Define("append")
    public static Object append(ListStruct args) {
        final ListStruct ret = new ListStruct();
        for (Object arg : args) {
            if (null == arg) continue;
            if(isList(arg)) {
                final ListStruct listStruct = (ListStruct) arg;
                for (Object iArg : listStruct) {
                    ret.add(iArg);
                }
            } else {
                ret.add(arg);
            }
        }
        return ret;
    }

    @Define("car")
    public static Object car(ListStruct args) throws Exception {
        return asList(args.car()).car();
    }

    @Define("nth")
    public static Object nth(ListStruct args) throws Exception {
        return asList(args.cadr()).nth(asInt(args.car()));
    }

    @Define("cdr")
    public static Object cdr(ListStruct args) throws Exception {
        return asList(args.car()).cdr();
    }

    @Define("nthcdr")
    public static Object nthcdr(ListStruct args) throws Exception {

        return asList(args.cadr()).nthcdr(asInt(args.car()));
    }

    @Define("cons")
    public static Object cons(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object a = args.car();
        final Object b = args.cadr();
        return cons(a, b);
    }

    private static Object cons (Object a, Object b) {
        if (TypeHelper.isEmptyList(a) && TypeHelper.isEmptyList(b)) {
            return new ListStruct();
        }
        if (TypeHelper.isEmptyList(b)) {
            return new ListStruct(a);
        }
        if (isList(b)) {
            if(TypeHelper.isEmptyList(a)) {
                return new ListStruct(null).setCdr((ListStruct) b);
            }
            return new ListStruct(a).setCdr((ListStruct) b);
        }
        return new ListStruct(a, b);
    }
}
