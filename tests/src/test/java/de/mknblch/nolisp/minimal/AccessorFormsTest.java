package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

public class AccessorFormsTest extends AbstractFormTest {


    @Test
    public void testAppend() throws Exception {
        final String code = "(eval (append nil + '(-2 -1) nil '(1 2 3 4 5 6 7 8 9)))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 ]", evaluated);
    }

    @Test
    public void testCar() throws Exception {
        final String code = "(car '(42 21 7))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 ]", evaluated);
    }

    @Test
    public void testCdr() throws Exception {

        final String code = "(cdr (cdr '(7 21 42)))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( 42 ) ]", evaluated);
    }

    @Test
    public void testCons() throws Exception {

        final String code = "(cons 1 2)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( 1 2 ) ]", evaluated);
    }

    @Test
    public void testConsEmptyList() throws Exception {

        final String code = "(cons 1 '())";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( 1 ) ]", evaluated);
    }

    @Test
    public void testConsEmptyLists() throws Exception {

        final String code = "(cons '() '())";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( nil ) ]", evaluated);
    }

    @Test
    public void testConsNilNil() throws Exception {

        final String code = "(cons nil nil)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( nil ) ]", evaluated);
    }

    @Test
    public void testConsNilAtEnd() throws Exception {

        final String code = "(cons 1 nil)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( 1 ) ]", evaluated);
    }


    @Test
    public void testConsNilAtStart() throws Exception {

        final String code = "(cons nil 1)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( nil 1 ) ]", evaluated);
    }

    @Test
    public void testConsNilAtStartListAtEnd() throws Exception {

        final String code = "(cons null '(1 2 3))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( nil 1 2 3 ) ]", evaluated);
    }

    @Test
    public void testConsEmptyListAtStartListAtEnd() throws Exception {

        final String code = "(cons '() '(1 2 3))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( nil 1 2 3 ) ]", evaluated);
    }

    @Test
    public void testCons2() throws Exception {

        final String code = "(cons 1 '(2 3))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( 1 2 3 ) ]", evaluated);
    }

    @Test
    public void testCons3() throws Exception {

        final String code = "(cons '(1) '(2 3))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( ( 1 ) 2 3 ) ]", evaluated);
    }

    @Test
    public void testNthcdr0() throws Exception {

        final String code = "(nthcdr 0 '(0 1 2))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( 0 1 2 ) ]", evaluated);
    }

    @Test
    public void testNthcdrN() throws Exception {

        final String code = "(nthcdr 1 '(0 1 2))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ ( 1 2 ) ]", evaluated);
    }

    @Test
    public void testNthcdrOverflow() throws Exception {

        final String code = "(nthcdr 4 '(0 1 2))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ nil ]", evaluated);
    }

    @Test
    public void testNth() throws Exception {

        final String code = "(nth 0 '(42 21 7))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 ]", evaluated);
    }

    @Test
    public void testNthN() throws Exception {

        final String code = "(nth 1 '(0 42 2))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 ]", evaluated);
    }

    @Test
    public void testNthOverflow() throws Exception {

        final String code = "(nth 5 '(0 42 2))";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ nil ]", evaluated);
    }

    @Test
    public void testArrayGet() throws Exception {

        final String code = "(aget (aget (aget [7 [21 [42]]] 1) 1) 0)";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ 42 ]", evaluated);
    }

    @Test
    public void testArraySet() throws Exception {

        final String code = "(let ((a [1 2 3 4])) {progn (aset a 0 42) (aset a 1 42) (aset a 2 42) (aset a 3 42) a } ) ";
        final List<Object> evaluated = eval(code);
        AbstractFormTest.assertASTEquals("L[ A[ 42 42 42 42 ] ]", evaluated);
    }

    @Test
    public void testArrayMake() throws Exception {

        final List<Object> evaluated = eval("(amake 3) ");
        AbstractFormTest.assertASTEquals("L[ A[ nil nil nil ] ]", evaluated);
    }

    @Test
    public void testArrayMultiSet() throws Exception {

        final List<Object> evaluated = eval("(progn (setq a (array-make 2)) (aset a 0 42 1 42) )");
        AbstractFormTest.assertASTEquals("L[ A[ 42 42 ] ]", evaluated);
    }
}