package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Constant;
import de.mknblch.nolisp.scanner.Define;
import de.mknblch.nolisp.scanner.Special;

/**
 * @author mknblch
 */
public class ConditionForms {


    @Constant({"true", "TRUE"})
    public static final boolean CONSTANT_TRUE = true;

    @Constant({"false", "FALSE"})
    public static final boolean CONSTANT_FALSE = false;

}
