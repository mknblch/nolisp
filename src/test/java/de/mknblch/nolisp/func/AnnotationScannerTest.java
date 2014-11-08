package de.mknblch.nolisp.func;

import de.mknblch.nolisp.ast.forms.Function;
import de.mknblch.nolisp.func.testforms.*;
import org.junit.Test;

import java.util.Set;

import static org.junit.Assert.assertEquals;


/**
 * @author mknblch
 */
public class AnnotationScannerTest {

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_WrongSignature() throws Exception {
        AnnotationScanner.scanMethods(WrongSignature.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NoReturnType() throws Exception {
        AnnotationScanner.scanMethods(NoReturnType.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_NotStatic() throws Exception {
        AnnotationScanner.scanMethods(NotStatic.class);
    }

    @Test(expected = FunctionDefinitionException.class)
    public void shouldFail_functionRedefinition() throws Exception {
        AnnotationScanner.scanMethods(DuplicateMethods.class);
    }

    @Test
    public void shouldWork_registerValid() throws Exception {

        final Set<Function> scan = AnnotationScanner.scanMethods(Working.class);
        assertEquals(4, scan.size());
    }
}