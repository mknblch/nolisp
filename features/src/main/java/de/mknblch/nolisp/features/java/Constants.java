package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.generator.Constant;

/**
 * @author mknblch
 */
public class Constants {

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
}
