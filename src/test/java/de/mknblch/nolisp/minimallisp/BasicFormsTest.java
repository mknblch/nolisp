package de.mknblch.nolisp.minimallisp;

import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class BasicFormsTest extends AbstractFormTest {

    @Test
    public void testQuote() throws Exception {
        List<Object> result = eval("(quote 1)");
        dump(result);
        assertEquals(1, result.get(0));
    }

    @Test(expected = EvaluationException.class)
    public void testLet() throws Exception {
        eval("(let ((a (+ 1 2)) (b a)) b)");
    }

    @Test
    public void testLetAsterisk() throws Exception {
        List<Object> result = eval("(let* ((a (+ 1 2)) (b a)) b)");
        dump(result);
        assertEquals(3, result.get(0));
    }

    @Test
    public void testSetq() throws Exception {
        List<Object> result = eval("(setq c 1) c");
        dump(result);
        assertEquals(1, result.get(0));
    }

    @Test
    public void testSetqList() throws Exception {
        List<Object> result = eval("(setq a 1 b (* 3 a) c (* a b))");
        dump(result);
        assertEquals(3, result.get(0));
    }

    @Test
    public void testProgn() throws Exception {
        final List<Object> result = eval("(progn 0 7 14 21 28 35 42)");
        assertEquals(42, result.get(0));
    }

    @Test
    public void testPrognEmpty() throws Exception {
        final List<Object> result = eval("(progn)");
        assertEquals(null, result.get(0));
    }

    @Test
    public void testList() throws Exception {
        final List<Object> result = eval("(list 1 (+ 1 1) (+ 1 1 1) (+ 1 1 1 1))");
        assertASTEquals("( 1 2 3 4 )", result.get(0));
    }

    @Test
    public void testList2() throws Exception {
        final List<Object> result = eval("(list (list (list)))");
        assertASTEquals("L[ ( ( nil ) ) ]", result);
    }

    @Test(expected = EvaluationException.class)
    public void testBadSyntaxList() throws Exception {
        eval("(0 1 2 3)");
    }
}
