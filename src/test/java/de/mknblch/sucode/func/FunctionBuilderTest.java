package de.mknblch.sucode.func;

import de.mknblch.sucode.ast.Function;
import de.mknblch.sucode.func.testforms.*;
import org.junit.Test;

import java.util.Set;

import static junit.framework.Assert.assertEquals;
import static junit.framework.TestCase.assertTrue;

/**
 * Created by mknblch on 11.10.2014.
 */
public class FunctionBuilderTest {

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_WrongSignature() throws Exception {
        FunctionBuilder.build(WrongSignature.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NoReturnType() throws Exception {
        FunctionBuilder.build(NoReturnType.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NotStatic() throws Exception {
        FunctionBuilder.build(NotStatic.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_functionRedefinition() throws Exception {
        FunctionBuilder.build(DuplicateMethods.class);
    }

    @Test
    public void shouldWork_registerValid() throws Exception {

        final Set<Function> scan = FunctionBuilder.build(Working.class);
        assertEquals(4, scan.size());
    }
}
