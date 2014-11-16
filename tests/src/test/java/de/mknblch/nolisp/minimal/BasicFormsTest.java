package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Language;
import de.mknblch.nolisp.core.minimal.Minimal;
import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
import de.mknblch.nolisp.core.interpreter.Context;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        final Context env = new Context(new Minimal());
        env.bind("x", 3);
        final List<Object> evaluated = eval(code, loggingInterpreter, env);
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
        assertEquals(1, result.get(0));
    }

    @Test(expected = EvaluationException.class)
    public void testLet() throws Exception {
        eval("(let ((a (+ 1 2)) (b a)) b)");
    }

    @Test
    public void testLetAsterisk() throws Exception {
        List<Object> result = eval("(let* ((a (+ 1 2)) (b a)) b)");
        dump(result);
        assertEquals(3, result.get(0));
    }

    @Test
    public void testSetq() throws Exception {
        List<Object> result = eval("(setq c 1) c");
        dump(result);
        assertEquals(1, result.get(0));
    }

    @Test
    public void testSetqList() throws Exception {
        List<Object> result = eval("(setq a 1 b (* 3 a) c (* a b))");
        dump(result);
        assertEquals(3, result.get(0));
    }

    @Test
    public void testProgn() throws Exception {
        final List<Object> result = eval("(progn 0 7 14 21 28 35 42)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testPrognEmpty() throws Exception {
        final List<Object> result = eval("(progn)");
        assertEquals(null, result.get(0));
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
