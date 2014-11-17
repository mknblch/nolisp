package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
import org.junit.Test;

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


        eval("(try (throw (new java.lang.Exception)) (catch java.lang.Exception e (...))  )");

    }
}