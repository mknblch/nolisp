package de.mknblch.nolisp.minilisp.forms;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.minilisp.testHelper.AbstractFormTest;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class BasicFormsTest extends AbstractFormTest {


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
        assertEquals(2, evaluated.get(0));
    }

    @Test
    public void testAddDouble() throws Exception {
        final String code = "(+ 1.0 1.0)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        assertEquals(2.0, evaluated.get(0));
    }

    @Test
    public void testAddMixed() throws Exception {
        final String code = "(+ 1 1.0)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        assertEquals(2.0, evaluated.get(0));
    }

//    @Test
//    public void testQuote() throws Exception {
//        final String code = "'a";
//        final List<Object> evaluated = eval(code);
//        dump(evaluated);
//    }

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
        assertEquals(1, evaluated.size());
        assertEquals(null, evaluated.get(0));
    }

    @Test
    public void testEnvironment() throws Exception {
        final String code = "x";
        final Context env = new Context();
        env.bind("x", 3);
        final List<Object> evaluated = eval(code, LOGGING_INTERPRETER, env);
        dump(evaluated);
        assertEquals(3, evaluated.get(0));
    }

    @Test
    public void test() throws Exception {
        final String code = ";hallo\n'a";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
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
}