package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.minimal.testHelper.AbstractFormTest;
import org.junit.Test;

import java.nio.charset.Charset;
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
    public void testClassOf() throws Exception {

        final List<Object> eval = eval("(classOf \"hallo\")");
        assertASTEquals("L[ class java.lang.String ]", eval);
    }

    @Test
    public void testClass() throws Exception {

        final List<Object> eval = eval("(class java.lang.String )");
        assertASTEquals("L[ class java.lang.String ]", eval);
    }

    @Test
    public void testCallArgs() throws Exception {

        final List<Object> eval = eval("(call concat (\"world\") \"hello \")");
        assertASTEquals("L[ hello world ]", eval);
    }

    @Test
    public void testCall() throws Exception {

        final List<Object> eval = eval("(call length \"just an overlong hello world thingy blblbl\")");
        assertASTEquals("L[ 42 ]", eval);
    }

    @Test
    public void testCallStaticNoArgs() throws Exception {

        final List<Object> eval = eval("(call-static java.nio.charset.Charset:defaultCharset)");
        assertTrue(eval.get(0) instanceof Charset);
    }

    @Test
    public void testCallStatic() throws Exception {

        final List<Object> eval = eval("(call-static java.lang.Integer:parseInt (\"42\"))");
        assertASTEquals("L[ 42 ]", eval);
    }

    @Test
    public void testCallPrimitiveTyp() throws Exception {
        final List<Object> eval = eval("(call-static java.lang.Math:abs (int) (-42))");
        assertASTEquals("L[ 42 ]", eval);
    }

    @Test
    public void testCallVarargs() throws Exception {

        // varargs is tricky!
        final List<Object> eval = eval("(call-static java.lang.String:format ( string array ) (\"%04d\" (amake 42)))");

        assertASTEquals("L[ 0042 ]", eval);
    }
}