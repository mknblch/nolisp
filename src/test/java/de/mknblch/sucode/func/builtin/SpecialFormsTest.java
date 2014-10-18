package de.mknblch.sucode.func.builtin;

import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.structs.ConstStruct;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 12.10.2014.
 */
public class SpecialFormsTest extends AbstractFormTest {

    @Test
    public void testQuote() throws Exception {
        List<Object> result = eval("(quote 1)");
        dump(result);
        System.out.println(result.get(0).getClass().getSimpleName());
        assertTrue(result.get(0) instanceof ConstStruct);
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
    public void testLambda() throws Exception {
        List<Object> result = eval("((lambda (a b) (+ a b)) 1 2 )");
        dump(result);
        assertEquals(3, result.get(0));
    }

    @Test//(expected = EvaluationException.class)
    public void testEmptyLambda() throws Exception {
        List<Object> result = eval("( ((lambda () 1)))");
        dump(result);
        assertEquals(1, result.get(0));
    }
}
