package de.mknblch.sucode.builtin;

import de.mknblch.sucode.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class PredicateFormsTest extends AbstractFormTest {

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
