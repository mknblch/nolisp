package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Define;

import java.util.ArrayList;

import static de.mknblch.nolisp.common.TypeHelper.*;

/**
 * @author mknblch
 */
public class ArrayForms {

    /**
     * retrieve nth element of an array
     * usage: (aget [42 21 7] 0) => 42
     */
    @Define({"aget", "array-get"})
    public static Object aget(ListStruct args) throws Exception {
        return asArray(args.car()) [ asInt(args.cadr()) ];
    }

    @Define({"aset", "array-set"}) // (aset (ainit 2) 1 "welt" 0 "hallo") => "hallo welt"
    public static Object[] aset(ListStruct args) throws Exception {
        final Object[] objects = asArray(args.car());
        ListStruct temp = args.cdr();
        while (null != temp) {
            final int index = asInt(temp.car());
            Expectations.expectCdr(temp);
            objects[ index ] = temp.cadr();
            temp = temp.cddr();
        }
        return objects;
    }

    @Define({"ainit", "array-init"}) // (ainit 3) => [null, null, null]
    public static Object ainit(ListStruct args) throws Exception {
        return new Object[asInt(args.car())];
    }

    @Define({"amake", "array-make"}) // (amake 1 2 3) => [1, 2, 3]
    public static Object amake(ListStruct args) throws Exception {
        if (null == args) {
            return new Object[0];
        }
        final ArrayList<Object> objects = new ArrayList<>();
        for (Object arg : args) {
            objects.add(arg);
        }
        return objects.toArray();
    }
}
