package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
import org.junit.Ignore;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

public class JavaFormsTest extends AbstractFormTest{

    @Test
    public void testThrow() throws Exception {
        boolean ex = false;
        try {
            eval("(throw (new java.lang.Exception))");
        } catch (Exception e) {
            ex = true;
        }
        assertTrue("Exception not thrown", ex);
    }

    @Test
    public void testThrowMessage() throws Exception {
        boolean ex = false;
        try {
            eval("(throw (new de.mknblch.nolisp.core.interpreter.EvaluationException (\"Test\")))");
        } catch (EvaluationException e) {
            ex = true;
            assertEquals("Test", e.getMessage());
        }
        assertTrue("Exception not thrown", ex);
    }

    @Test
    public void testNew() throws Exception {
        assertASTEquals("L[ Test ]", eval("(new java.lang.String (\"Test\"))"));

    }

    @Test
    public void testTry() throws Exception {
        final List<Object> eval = eval(
                "(try " +
                    "(throw (new java.lang.Exception)) " +
                        "(" +
                            "(catch java.lang.ArithmeticException e 0)" +
                            "(catch java.lang.Exception e 42)))");

        assertASTEquals("L[ 42 ]", eval);

    }

    @Test
    public void testTryBasicException() throws Exception {

        final List<Object> eval = eval(
                "(try " +
                        "(/ 1 0)" +
                        "((catch java.lang.Exception e e)))");

        assertASTEquals("L[ java.lang.ArithmeticException: / by zero ]", eval);
    }

    @Test(expected = ClassNotFoundException.class)
    public void testBadException() throws Exception {

        final List<Object> eval = eval(
                "(try " +
                        "(/ 1 0)" +
                        "((catch java.lang.BadbadException oO 42)))");
    }

    @Test
    public void testCallArgs() throws Exception {

        final List<Object> eval = eval("(call \"hello \" concat (\"world\"))");
        assertASTEquals("L[ hello world ]", eval);
    }

    @Test
    public void testCall() throws Exception {

        final List<Object> eval = eval("(call \"just an overlong hello world thingy blblbl\" length)");
        assertASTEquals("L[ 42 ]", eval);
    }

    @Test
    public void testCallStatic() throws Exception {

        final List<Object> eval = eval("(call-static java.lang.Integer.parseInt(\"42\"))");
        assertASTEquals("L[ 42 ]", eval);
    }

    @Ignore // TODO impl a way to call methods with varargs
    @Test
    public void testCallVarargs() throws Exception {

        final List<Object> eval = eval("(call-static java.lang.String.format(\"%04d\" 42))");

        assertASTEquals("L[ 42 ]", eval);
    }

    @Ignore // TODO implement a way to call methods with primitive type
    @Test
    public void testCallPrimitiveTyp() throws Exception {
        final List<Object> eval = eval("(call-static java.lang.Math.abs (-42))");
        assertASTEquals("L[ 42 ]", eval);
    }
}