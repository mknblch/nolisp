package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.func.AbstractFormTest;
import de.mknblch.sucode.parser.Parser;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 09.10.2014.
 */
public class InterpreterTest extends AbstractFormTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(InterpreterTest.class);
    private static final Parser PARSER = new Parser();

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
    public void testEnvironment() throws Exception {
        final String code = "(+ 2 x)";
        final Context env = new Context();
        env.bind("x", 3);
        final List<Object> evaluated = eval(code, env);
        dump(evaluated);
        assertEquals(5, evaluated.get(0));
    }


}
