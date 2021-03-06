package de.mknblch.nolisp.features.minimal.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

import java.security.SecureRandom;
import java.util.Random;

/**
 * @author mknblch
 */
@Define({"random-int", "rint"})
public class RandomIntForm implements Form  {

    private static final Random SECURE_RANDOM = new SecureRandom();

    @Override
    public Object eval(ListStruct args) throws Exception {
        final int arint = Math.abs(SECURE_RANDOM.nextInt());
        if (null == args) return arint;
        final int max = TypeHelper.asInt(args.car());
        return max > 0 ? arint % max : 0;
    }
}    