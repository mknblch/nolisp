package de.mknblch.sucode.func;

import de.mknblch.sucode.func.testforms.*;
import de.mknblch.sucode.interpreter.DefaultInterpreter;
import de.mknblch.sucode.interpreter.Interpreter;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

import static junit.framework.Assert.assertEquals;
import static junit.framework.TestCase.assertTrue;

/**
 * Created by mknblch on 11.10.2014.
 */
public class FunctionBuilderTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(FunctionBuilderTest.class);
    private static final Interpreter interpreter = new DefaultInterpreter();

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_WrongSignature() throws Exception {
        FunctionBuilder.build(interpreter, WrongSignature.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NoReturnType() throws Exception {
        FunctionBuilder.build(interpreter, NoReturnType.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NotStatic() throws Exception {
        FunctionBuilder.build(interpreter, NotStatic.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_functionRedefinition() throws Exception {
        FunctionBuilder.build(interpreter, DuplicateMethods.class);
    }

    @Test
    public void shouldWork_registerValid() throws Exception {

        final Set<Function> scan = FunctionBuilder.build(interpreter, Working.class);
        assertEquals(4, scan.size());
    }
}
