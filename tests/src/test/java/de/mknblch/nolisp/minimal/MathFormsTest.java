package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class MathFormsTest extends AbstractFormTest {

    @Test
    public void testPlus() throws Exception {
        final List<Object> result = eval("(+ 1 (+ 2 3 (+ 4 5 6 7 8 6)))");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testMinus() throws Exception {

        final List<Object> result = eval("(- -42 (- 42 126))");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testMul() throws Exception {

        final List<Object> result = eval("(* 2 (* 3 7))");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testDiv() throws Exception {

        final List<Object> result = eval("(/ (/ (/ 336 2) 2) 2)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testIntDiv() throws Exception {

        final List<Object> result = eval("(/ 84 2)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testMixedDiv() throws Exception {

        final List<Object> result = eval("(/ 84.84 2)");
        assertEquals(42.42, result.get(0));
    }

    @Test
    public void testMod() throws Exception {

        final List<Object> result = eval("(% 128 86)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testPow() throws Exception {

        final List<Object> result = eval("(** 6.481 2)");
        assertEquals(42d, (Double) result.get(0), 0.01);
    }

}
