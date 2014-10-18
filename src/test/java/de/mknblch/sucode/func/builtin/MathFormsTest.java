package de.mknblch.sucode.func.builtin;

import de.mknblch.sucode.func.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 12.10.2014.
 */
public class MathFormsTest extends AbstractFormTest {

    @Test
    public void testPlus() throws Exception {
        final List<Object> result = eval("(+ 1 (+ 2 3 (+ 4 5 6 7 8 6)))");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testMinus() throws Exception {

        final List<Object> result = eval("(- 0 (- 2 1))");
        assertEquals(-1, result.get(0));
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

        final List<Object> result = eval("(/ 3 2)");
        assertEquals(1, result.get(0));
    }

    @Test
    public void testMixedDiv() throws Exception {

        final List<Object> result = eval("(/ 3 2.0)");
        assertEquals(1.5, result.get(0));
    }

    @Test
    public void testMod() throws Exception {

        final List<Object> result = eval("(% 128 86)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testPow() throws Exception {

        final List<Object> result = eval("(** 2 8)");
        assertEquals(256d, result.get(0));
    }

}
