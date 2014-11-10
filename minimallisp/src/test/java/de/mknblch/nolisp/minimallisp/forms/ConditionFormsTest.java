package de.mknblch.nolisp.minimallisp.forms;

import de.mknblch.nolisp.minimallisp.MinimalLisp;
import de.mknblch.nolisp.minimallisp.testHelper.AbstractFormTest;
import de.mknblch.nolisp.core.interpreter.Context;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class ConditionFormsTest extends AbstractFormTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConditionFormsTest.class);

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
    public void testFori() throws Exception {
        final List<Object> result = eval("(setq a 1)(fori (0 12 2) (setq a (+ a a)))");
        AbstractFormTest.assertASTEquals("L[ 1 64 ]", result);
    }
    @Test
    public void testForiTime() throws Exception {

        callForI(10);
        callForI(10);
        callForI(100);
        callForI(1000);
        callForI(10000);
        callForI(100000);
        callForI(1000000);
    }

    public void callForI(int count) throws Exception {
        final String code =
                "(setq a 0)" +
                "(fori (0 " + count + ") " +
                        "(setq a (+ a 1)))";

        final long start = System.currentTimeMillis();
        eval(code, AbstractFormTest.CORE_INTERPRETER, new Context(new MinimalLisp()));
        final long end = System.currentTimeMillis();

        LOGGER.info("Done {} iterations in {}ms", count, (end - start));
    }

}
