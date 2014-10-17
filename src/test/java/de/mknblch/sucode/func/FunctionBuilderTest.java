package de.mknblch.sucode.func;

import de.mknblch.sucode.func.testforms.*;
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

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_WrongSignature() throws Exception {
        FunctionBuilder.scan(WrongSignature.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NoReturnType() throws Exception {
        FunctionBuilder.scan(NoReturnType.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NotStatic() throws Exception {
        FunctionBuilder.scan(NotStatic.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_functionRedefinition() throws Exception {
        FunctionBuilder.scan(DuplicateMethods.class);
    }

    @Test
    public void shouldWork_registerValid() throws Exception {

        final FunctionBuilder functionBuilder = new FunctionBuilder();
        final Set<Function> scan = functionBuilder.scan(Working.class);
        assertEquals(4, scan.size());
    }
}
