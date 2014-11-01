package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class MacroFormsTest extends AbstractFormTest {


    // TODO test name collision
    @Test
    public void testDefmacroComplex() throws Exception {

        final List<Object> result = eval("(defmacro s2 (a b v) (setq a v) (setq b v)) (setq x 0) (setq y 0) (s2 x y 1) x y (s2 x y 2) x y (s2 a b 3) a b");
        assertASTEquals("L[ s2 0 0 1 1 1 2 2 2 3 3 3 ]", result);
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

        final List<Object> result = eval("(defmacro aif (test then else) `(let ((it ,test)) (if it ,then ,else))) (aif (eq? 1 1) 1 2)");

        assertASTEquals("L[ aif 1 ]", result);

    }

}
