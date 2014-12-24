package de.mknblch.nolisp.features;

import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Assert;
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
        AbstractFormTest.assertASTEquals("L[ true ]", result);
    }

    @Test
    public final void testIsNotInt() throws Exception {
        final List<Object> result = eval("(int? 1.2)");
        AbstractFormTest.assertASTEquals("L[ false ]", result);
    }

    @Test
    public final void testIsReal() throws Exception {
        final List<Object> result = eval("(real? 1.2)");
        AbstractFormTest.assertASTEquals("L[ true ]", result);
    }

    @Test
    public final void testIsNotReal() throws Exception {
        final List<Object> result = eval("(real? nil)");
        AbstractFormTest.assertASTEquals("L[ false ]", result);
    }

    @Test
    public final void testIsString() throws Exception {
        final List<Object> result = eval("(string? \"hallo\")");
        AbstractFormTest.assertASTEquals("L[ true ]", result);
    }

    @Test
    public final void testIsNotString() throws Exception {
        final List<Object> result = eval("(string? 1.2)");
        AbstractFormTest.assertASTEquals("L[ false ]", result);
    }

    @Test
    public final void testIsList() throws Exception {
        final List<Object> result = eval("(list? '(abc))");
        Assert.assertEquals(true, result.get(0));
    }

    @Test
    public final void testIsNotList() throws Exception {
        final List<Object> result = eval("(list? 1.2)");
        AbstractFormTest.assertASTEquals("L[ false ]", result);
    }

    @Test
    public final void testInstaneof() throws Exception {
        final List<Object> result = eval("(instanceof? java.lang.Integer 42)");
        AbstractFormTest.assertASTEquals("L[ true ]", result);
    }

    @Test
    public final void testNotInstaneof() throws Exception {
        final List<Object> result = eval("(instanceof? java.lang.Integer 42.235)");
        AbstractFormTest.assertASTEquals("L[ false ]", result);

    }
}
