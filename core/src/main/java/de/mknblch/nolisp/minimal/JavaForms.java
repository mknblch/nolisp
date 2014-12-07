package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SymbolStruct;
import de.mknblch.nolisp.scanner.Constant;
import de.mknblch.nolisp.scanner.Define;
import de.mknblch.nolisp.scanner.Special;

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static de.mknblch.nolisp.common.TypeHelper.*;

/**
 * @author mknblch
 */
public class JavaForms {

    @Constant({"BOOLEAN", "boolean"})
    public static final Class<?> PRIMITIVE_BOOLEAN_TYPE = Boolean.TYPE;

    @Constant({"BYTE", "byte"})
    public static final Class<?> PRIMITIVE_BYTE_TYPE = Byte.TYPE;

    @Constant({"SHORT", "short"})
    public static final Class<?> PRIMITIVE_SHORT_TYPE = Short.TYPE;

    @Constant({"CHAR", "char"})
    public static final Class<?> PRIMITIVE_CHAR_TYPE = Character.TYPE;

    @Constant({"INT", "int"})
    public static final Class<?> PRIMITIVE_INT_TYPE = Integer.TYPE;

    @Constant({"LONG", "long"})
    public static final Class<?> PRIMITIVE_LONG_TYPE = Long.TYPE;

    @Constant({"FLOAT", "float"})
    public static final Class<?> PRIMITIVE_FLOAT_TYPE = Float.TYPE;

    @Constant({"DOUBLE", "double"})
    public static final Class<?> PRIMITIVE_DOUBLE_TYPE = Double.TYPE;

    @Constant({"STRING", "string"})
    public static final Class<?> PRIMITIVE_STRING_TYPE = String.class;

    @Constant({"ARRAY", "array"})
    public static final Class<?> PRIMITIVE_ARRAY_TYPE = Object[].class;

    @Constant({"cwd", "CWD"})
    public static final String CWD = System.getProperty("user.dir");

}
