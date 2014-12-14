package de.mknblch.nolisp.features.math;

import de.mknblch.nolisp.dialect.Constant;
import de.mknblch.nolisp.dialect.ContainsConstants;

/**
 * @author mknblch
 */
@ContainsConstants
public class Constants {

    @Constant("PI")
    public static final double PI = Math.PI;

    @Constant("E")
    public static final double E = Math.E;
}
