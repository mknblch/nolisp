package de.mknblch.nolisp.builtin;

import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class MacroFormsTest extends AbstractFormTest {


    // TODO check correctness
    @Test
    public void testDefmacroComplex() throws Exception {

        final List<Object> result = eval(
            "(defmacro s2 (a b v) `(setq ,a ,v ,b ,v))" +
                "(setq x 0 y 0)" +
                "(s2 x y 1)" +
                "x y " +
                "(setq x 0 y 0)" +
                "x y " +
                "(s2 x y 2) " +
                "x y"
        );
        assertASTEquals("L[ s2 0 1 1 1 0 0 0 2 2 2 ]", result);
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
        assertASTEquals("L[ 1 y 1 1 ]", result);

    }

    @Test
    public void testDefmacroBQ() throws Exception {

        final List<Object> result = eval("(defmacro aif (test then else) `(if ,test ,then ,else)) (aif 1 (setq y 0 x 1) (setq y 1 x 0)) x y");

        assertASTEquals("L[ aif 1 1 0 ]", result);

    }

}
