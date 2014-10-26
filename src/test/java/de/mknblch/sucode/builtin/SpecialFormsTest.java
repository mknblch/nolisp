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
        assertASTEquals("#<LAMBDA> (x) ( * 2 x )", result.get(0));
    }
    @Test
    public void testEvalBuiltin() throws Exception {
        final String code = "(eval +)";
        final List<Object> result = eval(code);
        assertASTEquals("#<BUILTIN +>", result.get(0));
    }

    @Test
    public void testList() throws Exception {
        final List<Object> result = eval("(list 1 (+ 1 1) (+ 1 1 1) (+ 1 1 1 1))");
        assertASTEquals("( 1 2 3 4 )", result.get(0));
    }

    // TODO review! maybe a dedicated function context is needed because +, ++ and +++ have special meaning in cl
    @Test
    public void testFunction() throws Exception {
        final List<Object> result = eval("+");
        assertASTEquals("#<BUILTIN +>", result.get(0));
    }

    // TODO test name collision
    @Test
    public void testDefmacroComplex() throws Exception {

        final List<Object> result = eval("(defmacro s2 (a b v) (setq a v) (setq b v)) (setq x 0) (setq y 0) (s2 x y 1) x y (s2 x y 2) x y");
        assertASTEquals("L[ nil 0 0 1 1 1 2 2 2 ]", result);
    }

    @Test
    public void testDefmacro() throws Exception {

        final List<Object> result = eval("(defmacro s2 () 42) (s2)");
        assertEquals(42, result.get(1));
    }
    @Test
    public void testDefmacro1() throws Exception {

        final List<Object> result = eval("(defmacro s2 (x y) (+ y x)) (s2 2 1)");
        assertEquals(3, result.get(1));
    }

    @Test
    public void testDefmacroEmpty() throws Exception {

        final List<Object> result = eval("(setq x 1) (defmacro y () x) x (y)");
        assertASTEquals("L[ 1 nil 1 1 ]", result);

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
    public void testProgn() throws Exception {
        final List<Object> result = eval("(progn 0 7 14 21 28 35 42)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testPrognEmpty() throws Exception {
        final List<Object> result = eval("(progn)");
        assertEquals(null, result.get(0));
    }

}
