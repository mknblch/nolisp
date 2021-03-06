package de.mknblch.nolisp.features;

import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class MathFormsTest extends AbstractFormTest {

    @Test
    public void testPlus() throws Exception {
        final List<Object> result = eval("(+ 1 (+ 2 3 (+ 4 5 6 7 8 6)))");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testMinus() throws Exception {

        final List<Object> result = eval("(- 10 5 3 1)");
        Assert.assertEquals(1, result.get(0));
    }

    @Test
    public void testMul() throws Exception {

        final List<Object> result = eval("(* 2 3 7)");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testDiv() throws Exception {

        final List<Object> result = eval("(/ (/ (/ 336 2) 2) 2)");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testIntDiv() throws Exception {

        final List<Object> result = eval("(/ 84 2)");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testMixedDiv() throws Exception {

        final List<Object> result = eval("(/ 84.84 2)");
        Assert.assertEquals(42.42, result.get(0));
    }

    @Test
    public void testMod() throws Exception {

        final List<Object> result = eval("(% 128 86)");
        Assert.assertEquals(42, result.get(0));
    }

    @Test
    public void testPow() throws Exception {

        final List<Object> result = eval("(** 6.481 2)");
        Assert.assertEquals(42d, (Double) result.get(0), 0.01);
    }

    @Test
    public void testRint() throws Exception {
        final List<Object> eval = eval("(rint 3)(rint)(rint 3)");
        dump(eval);
    }
    @Test
    public void testRint0() throws Exception {
        final List<Object> eval = eval("(rint 0)");
        AbstractFormTest.assertASTEquals("L[ 0 ]", eval);
    }
}
