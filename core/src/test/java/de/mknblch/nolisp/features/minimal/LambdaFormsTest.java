package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.features.lambda.Lambda;
import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertTrue;

/**
 * @author mknblch
 */
public class LambdaFormsTest extends AbstractFormTest {

    @Test
    public final void testRecursiveCall() throws Exception {
        final List<Object> result = eval("(setq f (lambda (n) (if (eq? n 0) 1 (* n (f (- n 1)))))) (f 5)");
        Assert.assertEquals(120, result.get(1));
    }

    @Test
    public final void testDefun() throws Exception {
        final List<Object> result = eval("(defun f (n) (if (eq? n 0) 1 (* n (f (- n 1))))) (f 5)");
        Assert.assertEquals(120, result.get(1));
    }

    @Test
    public final void testEval() throws Exception {
        final List<Object> result = eval("(eval '(+ 22 20))");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public final void testEvalBQ() throws Exception {
        final List<Object> result = eval("(eval `(+ 22 20))");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public final void testLArgs() throws Exception {
        final List<Object> result = eval("(largs (lambda (x y) (+ x y)))");
        assertASTEquals("L[ ( x y ) ]", result);
    }

    @Test
    public final void testLBody() throws Exception {
        final List<Object> result = eval("(lbody (lambda () (+ 1 1)))");
        assertASTEquals("L[ ( + 1 1 ) ]", result);
    }

    @Test
    public final void testLBody2() throws Exception {
        final List<Object> result = eval("(progn (defun x (n) (+ 1 1)) (lbody x))");
        assertASTEquals("L[ ( + 1 1 ) ]", result);
    }

    @Test
    public final void testEvalSymbol() throws Exception {
        final String code =
                "(setq x 42)" +
                        "(setq y x)" +
                        "(setq z y)" +
                        "(eval z)";

        final List<Object> result = eval(code);
        Assert.assertEquals(42, result.get(2));
    }

    @Test
    public final void testEvalQuotedList() throws Exception {
        final String code = "(setq x 3)" +
                "(setq p '(+ (* 3 (* x x)) 15))" +
                "(eval p)";
        final List<Object> result = eval(code);
        Assert.assertEquals(42, result.get(2));
    }

    @Test
    public void testEvalLambda() throws Exception {
        final String code = "(eval (lambda (x) (* 2 x)))";
        final List<Object> result = eval(code);
        AbstractFormTest.assertASTEquals("#<FORM> ( x ) ( * 2 x )", result.get(0));
    }

    @Test
    public void testEvalBuiltin() throws Exception {
        final String code = "(eval +)";
        final List<Object> result = eval(code);
        AbstractFormTest.assertASTEquals("#<BUILTIN +>", result.get(0));
    }

    @Test
    public void testFunction() throws Exception {
        final List<Object> result = eval("+");
        AbstractFormTest.assertASTEquals("#<BUILTIN +>", result.get(0));
    }

    @Test
    public void testFuncallPlus() throws Exception {
        final List<Object> result = eval("(funcall #'+ 3 4 5 6 7 8 9)");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testFuncallMinus() throws Exception {
        final List<Object> result = eval("(funcall #'- 50 1 1 1 1 1 1 1 1)");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testLambda() throws Exception {
        List<Object> result = eval("((lambda (a b) (+ a b)) 1 2 )");
        dump(result);
        Assert.assertEquals(3, result.get(0));
    }

    @Test
    public void testEmptyLambda() throws Exception {
        List<Object> result = eval("( (lambda () 1) )");
        dump(result);
        Assert.assertEquals(1, result.get(0));
    }

    @Test
    public void testLambdaFunc() throws Exception {
        List<Object> result = eval("(lambda () 1)");
        dump(result);
        assertTrue(result.get(0) instanceof Lambda);
    }

    @Test
    public void testSetqLambda() throws Exception {
        List<Object> result = eval("(setq c (lambda (a b) (+ a b))) (c 21 21)");
        dump(result);
        Assert.assertEquals(42, result.get(1));
    }

    @Test
    public void testLambda2() throws Exception {
        List<Object> result = eval(
                "(progn " +
                        "(defun odd? (n) (!= 0 (% n 2)))" +
                        "(defun foreach (f l) (if l (cons (f (car l)) (foreach f (cdr l))))) " +
                        "(foreach odd? '(1 2 3 4 5 6 7)))");
        assertASTEquals("L[ ( true false true false true false true ) ]", result);
    }



}
