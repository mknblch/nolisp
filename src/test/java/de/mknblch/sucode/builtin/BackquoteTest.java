package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.testHelper.AbstractFormTest;
import org.junit.Ignore;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class BackquoteTest extends AbstractFormTest {

    @Test
    public void testBackquoteOnNonList() throws Exception {
        final String code = "(setq a 42) a `a";
        final List<Object> evaluated = eval(code);
        assertASTEquals("L[ 42 42 a ]", evaluated);
    }

    @Test
    public void testBackquoteOnNonList2() throws Exception {
        final String code = "(setq c 42) `(a b (,c) d)";
        final List<Object> evaluated = eval(code);
        assertASTEquals("L[ 42 ( a b ( 42 ) d ) ]", evaluated);
    }

    @Test
    public void testCommaOnNonList() throws Exception {
        final String code = "(setq a 42) a `,a";
        final List<Object> evaluated = eval(code);
        assertASTEquals("L[ 42 42 42 ]", evaluated);
    }

    @Test
    public void testBackquoteList() throws Exception {
        final String code = "`(a b)";
        final List<Object> evaluated = eval(code);
        assertASTEquals("L[ ( a b ) ]", evaluated);
    }

}
