package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.scanner.Define;

import static de.mknblch.nolisp.core.common.TypeHelper.*;

/**
 * @author mknblch
 */
public class AccessorForms {

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
        final Object car = args.car();
        final Object cdar = args.cdar();
        if (isList(cdar)) {
            return new ListStruct(car).setCdr((ListStruct) cdar);
        }
        return new ListStruct(car, cdar);
    }

}
