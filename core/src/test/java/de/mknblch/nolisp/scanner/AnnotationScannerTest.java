package de.mknblch.nolisp.scanner;

import de.mknblch.nolisp.scanner.testforms.*;
import org.junit.Assert;
import org.junit.Test;

import java.util.Map;

/**
 * @author mknblch
 */
public class AnnotationScannerTest {

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_WrongSignature() throws Exception {
        AnnotationScanner.scanForFunctions(WrongSignature.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NoReturnType() throws Exception {
        AnnotationScanner.scanForFunctions(NoReturnType.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NotStatic() throws Exception {
        AnnotationScanner.scanForFunctions(NotStatic.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_functionRedefinition() throws Exception {
        AnnotationScanner.scanForFunctions(DuplicateMethods.class);
    }

    @Test
    public void shouldWork_registerValid() throws Exception {

        final Map<String, Object> scan = AnnotationScanner.scanForFunctions(Working.class);
        Assert.assertEquals(4, scan.size());
    }
}