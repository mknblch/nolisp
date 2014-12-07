package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Constant;
import de.mknblch.nolisp.scanner.Define;

import java.security.SecureRandom;

/**
 * @author mknblch
 */
public class MathForms {

    @Constant("PI")
    public static final double PI = Math.PI;

    @Constant("E")
    public static final double E = Math.E;


}
