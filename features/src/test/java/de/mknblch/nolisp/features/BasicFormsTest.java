package de.mknblch.nolisp.features;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.testHelper.AbstractFormTest;
import nolisp.Index;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

/**
 * @author mknblch
 */
public class BasicFormsTest extends AbstractFormTest {

    @Test
    public void testListLength() throws Exception {
        final List<Object> eval = eval("(llength '())");
        AbstractFormTest.assertASTEquals("L[ 0 ]", eval);
    }

    @Test
    public void testEmptyList() throws Exception {
        final List<Object> eval = eval("()");
        AbstractFormTest.assertASTEquals("L[ nil ]", eval);
    }


    @Test
    public void testPrint() throws Exception {
        final String code = "(print (+ 1 1(+ 2 3)))";
        eval(code);
    }

    @Test
    public void testAddInt() throws Exception {
        final String code = "(+ 1 1)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        Assert.assertEquals(2, evaluated.get(0));
    }

    @Test
    public void testAddDouble() throws Exception {
        final String code = "(+ 1.0 1.0)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        Assert.assertEquals(2.0, evaluated.get(0));
    }

    @Test
    public void testAddMixed() throws Exception {
        final String code = "(+ 1 1.0)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        Assert.assertEquals(2.0, evaluated.get(0));
    }

    @Test
    public void testComment() throws Exception {
        final String code = ";hallo\n'a";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
    }

    @Test
    public void testNull() throws Exception {
        final String code = "nil";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        Assert.assertEquals(1, evaluated.size());
        Assert.assertEquals(null, evaluated.get(0));
    }

    @Test
    public void testEnvironment() throws Exception {
        final String code = "x";
        final Context context = new Context().addDialect(Index.DIALECTS);
        context.bind("x", 3);
        final List<Object> evaluated = eval(code, AbstractFormTest.loggingInterpreter, context);
        dump(evaluated);
        Assert.assertEquals(3, evaluated.get(0));
    }

    @Test
    public void testQuote() throws Exception {
        List<Object> result = eval("(quote 1)");
        dump(result);
        Assert.assertEquals(1, result.get(0));
    }

    @Test(expected = EvaluationException.class)
    public void testLet() throws Exception {
        eval("(let ((a (+ 1 2)) (b a)) b)");
    }

    @Test
    public void testLetAsterisk() throws Exception {
        List<Object> result = eval("(let* ((a (+ 1 2)) (b a)) b)");
        dump(result);
        Assert.assertEquals(3, result.get(0));
    }

    @Test
    public void testSetq() throws Exception {
        List<Object> result = eval("(setq c 1) c");
        dump(result);
        Assert.assertEquals(1, result.get(0));
    }

    @Test
    public void testSetqList() throws Exception {
        List<Object> result = eval("(setq a 1 b (* 3 a) c (* a b))");
        dump(result);
        Assert.assertEquals(3, result.get(0));
    }

    @Test
    public void testProgn() throws Exception {
        final List<Object> result = eval("(progn 0 7 14 21 28 35 42)");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testPrognEmpty() throws Exception {
        final List<Object> result = eval("(progn)");
        Assert.assertEquals(null, result.get(0));
    }

    @Test
    public void testList() throws Exception {
        final List<Object> result = eval("(list 1 (+ 1 1) (+ 1 1 1) (+ 1 1 1 1))");
        AbstractFormTest.assertASTEquals("( 1 2 3 4 )", result.get(0));
    }

    @Test
    public void testList2() throws Exception {
        final List<Object> result = eval("(list (list (list)))");
        AbstractFormTest.assertASTEquals("L[ ( ( nil ) ) ]", result);
    }

    @Test(expected = EvaluationException.class)
    public void testBadSyntaxList() throws Exception {
        eval("(0 1 2 3)");
    }

    @Test
    public void testArray() throws Exception {

        final List<Object> result = eval("[1 [2 3] 4]");
        AbstractFormTest.assertASTEquals("L[ A[ 1 A[ 2 3 ] 4 ] ]", result);

    }

    @Ignore
    @Test
    public void testLoad() throws Exception {

        final List<Object> result = eval("(load \"./src/test/scripts/1337.nl\")");
        AbstractFormTest.assertASTEquals("L[ ( 1337 ) ]", result);
    }

    @Ignore
    @Test
    public void testLoad2() throws Exception {

        final List<Object> result = eval("CWD (load \"./src/test/scripts/sentence.nl\")");
        dump(result);
    }

    @Test
    public void testCwd() throws Exception {

        final List<Object> result = eval("cwd");
        dump(result);
    }

    @Test
    public void testFor() throws Exception {
        final List<Object> result = eval("(for (x 0 10) ((print x)))");
        AbstractFormTest.assertASTEquals("L[ 9 ]", result);
    }

    @Test
    public void testWhile() throws Exception {
        final List<Object> result = eval("(setq n 0) (while (< n 10) ((setq n (+ n 1))))");
        AbstractFormTest.assertASTEquals("L[ 0 10 ]", result);
    }
}
