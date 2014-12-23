package de.mknblch.nolisp.features;

import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class MacroFormsTest extends AbstractFormTest {

    @Test
    public void testDefmacroComplex() throws Exception {

        final List<Object> result = eval(
            "(defmacro s2 (a b v) `(setq ,a ,v ,b ,v))" +
                "(setq x 0 y 0)" +
                "(s2 x y 1)" +
                "x y " +
                "(setq x 0 y 0)" +
                "x y " +
                "(s2 x y (+ 1 1)) " +
                "x y"
        );
        AbstractFormTest.assertASTEquals("L[ s2 0 1 1 1 0 0 0 2 2 2 ]", result);
    }

    @Test
    public void testDefmacro() throws Exception {

        final List<Object> result = eval("(defmacro s2 () 42) (s2)");
        Assert.assertEquals(42, result.get(1));
    }

    @Test
    public void testDefmacro1() throws Exception {

        final List<Object> result = eval("(defmacro s2 (x y) (+ y x)) (s2 2 1)");
        Assert.assertEquals(3, result.get(1));
    }
    @Test
    public void testDefmacroEmpty() throws Exception {

        final List<Object> result = eval("(setq x 1) (defmacro y () x) x (y)");
        AbstractFormTest.assertASTEquals("L[ 1 y 1 1 ]", result);

    }

    @Test
    public void testDefmacroBQ() throws Exception {

        final List<Object> result = eval("(defmacro aif (test then else) `(if ,test ,then ,else)) (aif 1 (setq y 0 x 1) (setq y 1 x 0)) x y (aif false (setq y 0 x 1) (setq y 1 x 0)) x y");

        AbstractFormTest.assertASTEquals("L[ aif 1 1 0 0 0 1 ]", result);

    }

}
