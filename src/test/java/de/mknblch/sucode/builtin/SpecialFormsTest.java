package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.func.AbstractFormTest;
import de.mknblch.sucode.interpreter.EvaluationException;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class SpecialFormsTest extends AbstractFormTest {

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
    public void testLambda() throws Exception {
        List<Object> result = eval("((lambda (a b) (+ a b)) 1 2 )");
        dump(result);
        assertEquals(3, result.get(0));
    }

    @Test
    public void testEmptyLambda() throws Exception {
        final String as
                = "asdasd asda";

        List<Object> result = eval("((lambda () 1))");
        dump(result);
        assertEquals(1, result.get(0));
    }

    @Test
    public void testLambdaFunc() throws Exception {
        List<Object> result = eval("(lambda () 1)");
        dump(result);
        assertTrue(result.get(0) instanceof Function);
    }

    @Test
    public void testSetqLambda() throws Exception {
        List<Object> result = eval("(setq c (lambda (a b) (+ a b))) (c 3 5)");
        dump(result);
        assertEquals(8, result.get(1));
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
    public final void testRecursiveCall() throws Exception {
        final List<Object> result = eval("(setq f (lambda (n) (if (eq? n 0) 1 (* n (f (- n 1)))))) (f 5)");
        assertEquals(120, result.get(1));
    }

    @Test
    public final void testDefun() throws Exception {
        final List<Object> result = eval("(defun f (n) (if (eq? n 0) 1 (* n (f (- n 1))))) (f 5)");
        assertEquals(120, result.get(1));
    }

    @Test
    public final void testEval() throws Exception {
        final List<Object> result = eval("(eval '(+ 22 20))");
        assertEquals(42, result.get(0));
    }

    @Test
    public final void testEvalSymbol() throws Exception {
        final String code =
                "(setq x 42)" +
                        "(setq y x)" +
                        "(setq z y)" +
                        "(eval z)";

        final List<Object> result = eval(code);
        assertEquals(42, result.get(2));
    }

    @Test
    public final void testEvalQuotedList() throws Exception {
        final String code = "(setq x 3)" +
                "(setq p '(+ (* 3 (* x x)) 15))" +
                "(eval p)";
        final List<Object> result = eval(code);
        assertEquals(42, result.get(2));
    }

    @Test
    public void testEvalLambda() throws Exception {
        final String code = "(eval (lambda (x) (* 2 x)))";
        final List<Object> result = eval(code);
        assertASTEquals("#<LAMBDA> (x)", result.get(0));
    }
    @Test
    public void testEvalBuiltin() throws Exception {
        final String code = "(eval +)";
        final List<Object> result = eval(code);
        assertASTEquals("#<FORM>", result.get(0));
    }

    @Test
    public void testList() throws Exception {
        final List<Object> result = eval("(list 1 (+ 1 1) (+ 1 1 1) (+ 1 1 1 1))");
        assertASTEquals("( 1 2 3 4 )", result.get(0));
    }
}
