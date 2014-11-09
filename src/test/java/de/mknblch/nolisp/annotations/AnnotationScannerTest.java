package de.mknblch.nolisp.annotations;

import de.mknblch.nolisp.annotations.testforms.*;
import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.*;

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
        assertEquals(4, scan.size());
    }
}
