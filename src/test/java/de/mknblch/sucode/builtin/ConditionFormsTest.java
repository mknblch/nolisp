package de.mknblch.sucode.builtin;

import de.mknblch.sucode.func.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 19.10.2014.
 */
public class ConditionFormsTest extends AbstractFormTest {

    @Test
    public void testIf() throws Exception {
        final List<Object> result = eval("(if T 42 43)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testIfNot() throws Exception {
        final List<Object> result = eval("(if nil 123 42)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testHalfIf() throws Exception {
        final List<Object> result = eval("(if T 42)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testEquals() throws Exception {
        final List<Object> result = eval("(eq? (+ 2 40) (- 80 38))");
        assertEquals(true, result.get(0));
    }

    @Test
    public void testEqualsSymbol() throws Exception {
        final List<Object> result = eval("(eq? 'a 'a)");
        assertEquals(true, result.get(0));
    }

    @Test
    public void testCond() throws Exception {
        final List<Object> result = eval("(cond (nil 1) ((eq? 1 2) 2) (T 42))");
        assertEquals(42, result.get(0));
    }

    @Test
    public final void testIsInt() throws Exception {
        final List<Object> result = eval("(int? 1)");
        assertEquals(true, result.get(0));
    }

    @Test
    public final void testIsNotInt() throws Exception {
        final List<Object> result = eval("(int? 1.2)");
        assertEquals(false, result.get(0));
    }

    @Test
    public final void testIsReal() throws Exception {
        final List<Object> result = eval("(real? 1.2)");
        assertEquals(true, result.get(0));
    }

    @Test
    public final void testIsNotReal() throws Exception {
        final List<Object> result = eval("(real? nil)");
        assertEquals(false, result.get(0));
    }

    @Test
    public final void testIsString() throws Exception {
        final List<Object> result = eval("(string? \"hallo\")");
        assertEquals(true, result.get(0));
    }

    @Test
    public final void testIsNotString() throws Exception {
        final List<Object> result = eval("(string? 1.2)");
        assertEquals(false, result.get(0));
    }

    @Test
    public final void testIsList() throws Exception {
        final List<Object> result = eval("(list? '(abc))");
        assertEquals(true, result.get(0));
    }

    @Test
    public final void testIsNotList() throws Exception {
        final List<Object> result = eval("(list? 1.2)");
        assertEquals(false, result.get(0));
    }
}
