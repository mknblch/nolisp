package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInForm;

import java.security.SecureRandom;

/**
 * @author mknblch
 */
public class RandomIntForm extends BuiltInForm {

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    @Override
    public String[] getSymbols() {
        return new String[]{"random-int", "rint"};
    }

    @Override
    public Object eval(ListStruct args) throws Exception {
        final int arint = Math.abs(SECURE_RANDOM.nextInt());
        if (null == args) return arint;
        final int max = TypeHelper.asInt(args.car());
        return max > 0 ? arint % max : 0;
    }
}    