package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;

import static de.mknblch.nolisp.core.common.TypeHelper.*;

/**
 * @author mknblch
 */
public class AccessorForms {

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

    /**
     * retrieve nth element of an array
     * usage: (aget [42 21 7] 0) => 42
     */
    @Define({"aget", "array-get"})
    public static Object aget(ListStruct args) throws Exception {
        return TypeHelper.asArray(args.car()) [ asInt(args.cdar()) ];
    }

    @Define({"aset", "array-set"}) // (aset (amake 2) 0 "hallo" 1 "welt") => "hallo welt"
    public static Object[] aset(ListStruct args) throws Exception {
        final Object[] objects = asArray(args.car());
        ListStruct temp = args.cdr();
        while (null != temp) {
            final int index = asInt(temp.car());
            Expectations.expectCdr(temp);
            objects[ index ] = temp.cdar();
            temp = temp.cddr();
        }
        return objects;
    }

    @Define({"amake", "array-make"}) // (amake 3)
    public static Object amake(ListStruct args) throws Exception {
        return new Object[asInt(args.car())];
    }

    @Define("car")
    public static Object car(ListStruct args) throws Exception {
        return asList(args.car()).car();
    }

    @Define("nth")
    public static Object nth(ListStruct args) throws Exception {

        return asList(args.cdar()).nth(asInt(args.car()));
    }

    @Define("cdr")
    public static Object cdr(ListStruct args) throws Exception {
        return asList(args.car()).cdr();
    }

    @Define("nthcdr")
    public static Object nthcdr(ListStruct args) throws Exception {

        return asList(args.cdar()).nthcdr(asInt(args.car()));
    }

    @Define("cons")
    public static Object cons(ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object a = args.car();
        final Object b = args.cdar();
        return cons(a, b);
    }

    // TODO refactor
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
