package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class BackquoteTest extends AbstractFormTest {

    @Test
    public void testQuote() throws Exception {
        List<Object> result = eval("(quote 1)");
        dump(result);
        assertEquals(1, result.get(0));
    }


    @Test
    public void testBackquoteOnNonList() throws Exception {
        final String code = "(setq a 42) `a `,a a";
        final List<Object> evaluated = eval(code);
        assertASTEquals("L[ 42 a 42 42 ]", evaluated);
    }

    @Test
    public void testBackquoteList() throws Exception {
        final String code = "`(a b)";
        final List<Object> evaluated = eval(code);
        assertASTEquals("L[ ( a b ) ]", evaluated);
    }

}
