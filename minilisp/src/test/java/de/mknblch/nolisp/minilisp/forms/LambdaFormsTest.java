package de.mknblch.nolisp.minilisp.forms;

import de.mknblch.nolisp.ast.forms.LambdaForm;
import de.mknblch.nolisp.minilisp.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class LambdaFormsTest extends AbstractFormTest {

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
    public final void testEvalBQ() throws Exception {
        final List<Object> result = eval("(eval `(+ 22 20))");
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
        AbstractFormTest.assertASTEquals("#<LAMBDA> (x) ( * 2 x )", result.get(0));
    }

    @Test
    public void testEvalBuiltin() throws Exception {
        final String code = "(eval +)";
        final List<Object> result = eval(code);
        AbstractFormTest.assertASTEquals("#<BUILTIN +>", result.get(0));
    }

    // TODO review! maybe a dedicated function context is needed because +, ++ and +++ have special meaning in cl
    @Test
    public void testFunction() throws Exception {
        final List<Object> result = eval("+");
        AbstractFormTest.assertASTEquals("#<BUILTIN +>", result.get(0));
    }

    @Test
    public void testFuncallPlus() throws Exception {
        final List<Object> result = eval("(funcall #'+ 3 4 5 6 7 8 9)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testFuncallMinus() throws Exception {
        final List<Object> result = eval("(funcall #'- 50 1 1 1 1 1 1 1 1)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testLambda() throws Exception {
        List<Object> result = eval("((lambda (a b) (+ a b)) 1 2 )");
        dump(result);
        assertEquals(3, result.get(0));
    }

    @Test
    public void testEmptyLambda() throws Exception {
        List<Object> result = eval("( (lambda () 1) )");
        dump(result);
        assertEquals(1, result.get(0));
    }

    @Test
    public void testLambdaFunc() throws Exception {
        List<Object> result = eval("(lambda () 1)");
        dump(result);
        assertTrue(result.get(0) instanceof LambdaForm);
    }

    @Test
    public void testSetqLambda() throws Exception {
        List<Object> result = eval("(setq c (lambda (a b) (+ a b))) (c 3 5)");
        dump(result);
        assertEquals(8, result.get(1));
    }

}
