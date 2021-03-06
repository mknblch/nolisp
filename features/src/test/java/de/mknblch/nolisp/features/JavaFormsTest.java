package de.mknblch.nolisp.features;

import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Assert;
import org.junit.Test;

import java.awt.image.BufferedImage;
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
        Assert.assertTrue("Exception not thrown", ex);
    }

    @Test
    public void testThrowMessage() throws Exception {
        boolean ex = false;
        try {
            eval("(throw (new de.mknblch.nolisp.interpreter.EvaluationException (\"Test\")))");
        } catch (EvaluationException e) {
            ex = true;
            Assert.assertEquals("Test", e.getMessage());
        }
        Assert.assertTrue("Exception not thrown", ex);
    }

    @Test
    public void testNewNoArgs() throws Exception {
        AbstractFormTest.assertASTEquals("L[ java.lang.Exception ]", eval("(new java.lang.Exception)"));
    }

    @Test
    public void testNewAutoArgs() throws Exception {
        AbstractFormTest.assertASTEquals("L[ Test ]", eval("(new java.lang.String (\"Test\"))"));
    }

    @Test
    public void testNewDefineArgs() throws Exception {
        AbstractFormTest.assertASTEquals("L[ Test ]", eval("(new java.lang.String (STRING) (\"Test\"))"));
    }

    @Test(expected = NoSuchMethodException.class)
    public void testNewDefineArgsError() throws Exception {
        AbstractFormTest.assertASTEquals("L[ Test ]", eval("(new java.lang.String (INT) (\"Test\"))"));
    }

    @Test
    public void testTry() throws Exception {
        final List<Object> eval = eval(
                "(try " +
                    "(throw (new java.lang.Exception)) " +
                        "(" +
                            "(catch java.lang.ArithmeticException e 0)" +
                            "(catch java.lang.Exception e 42)))");

        AbstractFormTest.assertASTEquals("L[ 42 ]", eval);

    }

    @Test
    public void testTryBasicException() throws Exception {

        final List<Object> eval = eval(
                "(try " +
                        "(/ 1 0)" +
                        "((catch java.lang.Exception e e)))");

        AbstractFormTest.assertASTEquals("L[ java.lang.ArithmeticException: / by zero ]", eval);
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
        AbstractFormTest.assertASTEquals("L[ class java.lang.String ]", eval);
    }

    @Test
    public void testClass() throws Exception {

        final List<Object> eval = eval("(class java.lang.String )");
        AbstractFormTest.assertASTEquals("L[ class java.lang.String ]", eval);
    }

    @Test
    public void testCallArgs() throws Exception {

        final List<Object> eval = eval("(call concat (\"world\") \"hello \")");
        AbstractFormTest.assertASTEquals("L[ hello world ]", eval);
    }

    @Test
    public void testCall() throws Exception {

        final List<Object> eval = eval("(call length \"just an overlong hello world thingy blblbl\")");
        AbstractFormTest.assertASTEquals("L[ 42 ]", eval);
    }

    @Test
    public void testCallStaticNoArgs() throws Exception {

        final List<Object> eval = eval("(call-static java.nio.charset.Charset:defaultCharset)");
        Assert.assertTrue(eval.get(0) instanceof Charset);
    }

    @Test
    public void testCallStatic() throws Exception {

        final List<Object> eval = eval("(call-static java.lang.Integer:parseInt (\"42\"))");
        AbstractFormTest.assertASTEquals("L[ 42 ]", eval);
    }

    @Test
    public void testCallConstant() throws Exception {

        final List<Object> eval = eval("(java-const java.awt.image.BufferedImage:TYPE_INT_ARGB)");
        Assert.assertEquals(BufferedImage.TYPE_INT_ARGB, eval.get(0));
    }

    @Test
    public void testCallPrimitiveTyp() throws Exception {
        final List<Object> eval = eval("(call-static java.lang.Math:abs (int) (-42))");
        AbstractFormTest.assertASTEquals("L[ 42 ]", eval);
    }

    @Test
    public void testCallVarargs() throws Exception {
        // varargs is tricky!
        final List<Object> eval = eval("(call-static java.lang.String:format ( string array ) (\"%04d\" (amake 42)))");
        AbstractFormTest.assertASTEquals("L[ 0042 ]", eval);
    }

}