package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.func.AbstractFormTest;
import de.mknblch.sucode.parser.Parser;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class InterpreterTest extends AbstractFormTest {

    @Test
    public void testPrint() throws Exception {
        final String code = "(print (+ 1 1(+ 2 3)))";
        eval(code);
    }

    @Test
    public void testAddInt() throws Exception {
        final String code = "(+ 1 1)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        assertEquals(2, evaluated.get(0));
    }

    @Test
    public void testAddDouble() throws Exception {
        final String code = "(+ 1.0 1.0)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        assertEquals(2.0, evaluated.get(0));
    }

    @Test
    public void testAddMixed() throws Exception {
        final String code = "(+ 1 1.0)";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        assertEquals(2.0, evaluated.get(0));
    }

    @Test
    public void testQuote() throws Exception {
        final String code = "'a";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
    }

    @Test
    public void testComment() throws Exception {
        final String code = ";hallo\n'a";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
    }

    @Test
    public void testNull() throws Exception {
        final String code = "nil";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
        assertEquals(1, evaluated.size());
        assertEquals(null, evaluated.get(0));
    }

    @Test
    public void testEnvironment() throws Exception {
        final String code = "x";
        final Context env = new Context();
        env.bind("x", 3);
        final List<Object> evaluated = eval(code, env);
        dump(evaluated);
        assertEquals(3, evaluated.get(0));
    }

    @Test
    public void test() throws Exception {
        final String code = ";hallo\n'a";
        final List<Object> evaluated = eval(code);
        dump(evaluated);
    }


}
