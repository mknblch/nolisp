package de.mknblch.nolisp.minimal.forms;

import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
import org.junit.Ignore;
import org.junit.Test;

import java.util.List;

/**
 * @author mknblch
 */
public class BackquoteTest extends AbstractFormTest {

    @Test
    public void testBackquoteSimple() throws Exception {
        final String code = "`(a (+ 1 2) c)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a ( + 1 2 ) c ) ]", evaluated);
    }

    @Test
    public void testBackquoteSimple2() throws Exception {
        final String code = "`(a ,(+ 1 2) c)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a 3 c ) ]", evaluated);
    }

    @Test
    public void testBackquoteSimple3() throws Exception {
        final String code = "`(a (list 1 2) c)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a ( list 1 2 ) c ) ]", evaluated);
    }


    @Test
    public void testBackquoteSimple4() throws Exception {
        final String code = "`(a ,(list 1 (+ 2 3)) c)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a ( 1 5 ) c ) ]", evaluated);
    }


    @Test
    public void testBackquoteSimple5() throws Exception {
        final String code = "`(a ,(list 1 `(+ ,2 ,3)) c)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a ( 1 ( + 2 3 ) ) c ) ]", evaluated);
    }

    @Test
    public void testBackquoteOnNonList() throws Exception {
        final String code = "(setq a 42) a `a";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 42 a ]", evaluated);
    }

    @Test
    public void testBackquoteOnNonList2() throws Exception {
        final String code = "(setq c 42) `(a b (,c) d)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 ( a b ( 42 ) d ) ]", evaluated);
    }

    @Test
    public void testCommaOnNonList() throws Exception {
        final String code = "(setq a 42) a `,a";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 42 42 ]", evaluated);
    }

    @Test
    public void testBackquoteList() throws Exception {
        final String code = "`(a b)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a b ) ]", evaluated);
    }

    @Test
    public void testSplice() throws Exception {
        final String code = "(+ @(21 21))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 ]", evaluated);
    }

    @Ignore
    @Test
    public void testSpliceEval() throws Exception {
        final String code = "`(a @(,(+ 1 1) c))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a 2 c ) ]", evaluated);
    }

    @Ignore
    @Test
    public void testSpliceEvalComplex() throws Exception {
        final String code = "`(1 ,(+ 1 1) @(3 ,(+ @(2 2))))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( a 2 c ) ]", evaluated);
    }

}
