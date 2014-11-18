package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
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
                            "(catch java.lang.ArithmeticException 0)" +
                            "(catch java.lang.Exception 42)))");

        assertASTEquals("L[ 42 ]", eval);

    }

    @Test
    public void testTryBaseEx() throws Exception {

        final List<Object> eval = eval(
                "(try " +
                        "(/ 1 0)" +
                        "((catch java.lang.Exception 42)))");
    }

    @Test(expected = ClassNotFoundException.class)
    public void testBadException() throws Exception {

        final List<Object> eval = eval(
                "(try " +
                        "(/ 1 0)" +
                        "((catch java.lang.BadbadException 42)))");
    }
}