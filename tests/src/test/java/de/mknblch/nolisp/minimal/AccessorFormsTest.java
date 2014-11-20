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
}