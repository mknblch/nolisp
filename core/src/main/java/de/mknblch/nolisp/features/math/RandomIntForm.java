package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;
import de.mknblch.nolisp.dialect.Define;

import java.security.SecureRandom;
import java.util.Random;

/**
 * @author mknblch
 */
@Define({"random-int", "rint"})
public class RandomIntForm extends BuiltInForm  {

    private static final Random SECURE_RANDOM = new SecureRandom();

    @Override
    public Object eval(ListStruct args) throws Exception {
        final int arint = Math.abs(SECURE_RANDOM.nextInt());
        if (null == args) return arint;
        final int max = TypeHelper.asInt(args.car());
        return max > 0 ? arint % max : 0;
    }
}    