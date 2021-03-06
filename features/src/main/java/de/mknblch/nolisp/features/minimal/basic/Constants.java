package de.mknblch.nolisp.features.minimal.basic;

import de.mknblch.nolisp.generator.annotations.Constant;

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
