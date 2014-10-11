package de.mknblch.sucode.interpreter.func.builtin;

import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 12.10.2014.
 */
public class MathFormsTest extends AbstractFormTest {

    @Test
    public void testPlus() throws Exception {
        final List<Object> result = eval("(+ 1 2 3)");
        assertEquals(6, result.get(0));
    }

    @Test
    public void testMinus() throws Exception {

        final List<Object> result = eval("(- 1 2)");
        assertEquals(-1, result.get(0));
    }

    @Test
    public void testMul() throws Exception {

        final List<Object> result = eval("(* 2 3)");
        assertEquals(6, result.get(0));
    }

    @Test
    public void testDiv() throws Exception {

        final List<Object> result = eval("(/ 32 8)");
        assertEquals(4, result.get(0));
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

        final List<Object> result = eval("(% 7 5)");
        assertEquals(2, result.get(0));
    }

    @Test
    public void testPow() throws Exception {

        final List<Object> result = eval("(** 2 8)");
        assertEquals(256d, result.get(0));
    }

}
