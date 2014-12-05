package de.mknblch.nolisp.common;

import de.mknblch.nolisp.interpreter.EvaluationException;
import org.junit.Test;

import static org.junit.Assert.*;

public class TypeHelperTest {

    @Test
    public void testIsObjectType() throws Exception {

        assertTrue(TypeHelper.isObjectType(5, "java.lang.Integer"));
        assertFalse(TypeHelper.isObjectType(5, "java.lang.Double"));
        assertTrue(TypeHelper.isObjectType("a", "java.lang.String"));
        assertTrue(TypeHelper.isObjectType(1.5, "java.lang.Double"));
    }

    @Test
    public void testIsPrimitiveObjectType() throws Exception {

        assertTrue(TypeHelper.isPrimitiveObjectType(5, "Integer"));
        assertFalse(TypeHelper.isPrimitiveObjectType(5, "Double"));
        assertTrue(TypeHelper.isPrimitiveObjectType("a", "String"));
        assertTrue(TypeHelper.isPrimitiveObjectType(1.5, "Double"));
        assertTrue(TypeHelper.isPrimitiveObjectType(1.5, "Number"));
    }

    @Test
    public void testIsExceptionObjectType() throws Exception {

        assertTrue(TypeHelper.isPrimitiveObjectType(new EvaluationException(), "Exception"));
    }

    @Test
    public void testCastToType() throws Exception {

        assertTrue(TypeHelper.castToType(5, "java.lang.Number") instanceof Number);
    }

    @Test(expected = ClassCastException.class)
    public void testCastToTypeError() throws Exception {

        TypeHelper.castToType(5, "java.lang.String") ;
    }

}