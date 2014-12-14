package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.dialect.Constant;

/**
 * @author mknblch
 */
public class Constants {

    @Constant({"true", "TRUE"})
    public static final boolean CONSTANT_TRUE = true;

    @Constant({"false", "FALSE"})
    public static final boolean CONSTANT_FALSE = false;

    @Constant({"cwd", "CWD"})
    public static final String CWD = System.getProperty("user.dir");

}
