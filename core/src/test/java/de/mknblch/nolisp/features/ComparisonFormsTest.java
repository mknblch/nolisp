package de.mknblch.nolisp.features.minimal;

import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Test;

public class ComparisonFormsTest extends AbstractFormTest {

    @Test
    public void testEqual() throws Exception {

        assertASTEquals("L[ true ]", eval("(== 2 2)"));
        assertASTEquals("L[ true ]", eval("(== 1.999 1.999)"));
        assertASTEquals("L[ true ]", eval("(== 2.0 2)"));
        assertASTEquals("L[ true ]", eval("(== 1 1.0)"));
    }
    @Test
    public void testGreater() throws Exception {

        assertASTEquals("L[ true ]", eval("(> 2 1.999)"));
        assertASTEquals("L[ false ]", eval("(> 1 1.999)"));
    }

    @Test
    public void testGreaterEqual() throws Exception {

        assertASTEquals("L[ true ]", eval("(>= 1.999 1.999)"));
        assertASTEquals("L[ false ]", eval("(>= 1.998 1.999)"));
        assertASTEquals("L[ true ]", eval("(>= 42 2)"));
        assertASTEquals("L[ true ]", eval("(>= 42 42)"));
    }

    @Test
    public void testLower() throws Exception {

        assertASTEquals("L[ false ]", eval("(< 2 1.9999)"));
        assertASTEquals("L[ true ]", eval("(< 1 1.0001)"));
    }

    @Test
    public void testLowerEqual() throws Exception {

        assertASTEquals("L[ true ]", eval("(<= 1.999 1.999)"));
        assertASTEquals("L[ false ]", eval("(<= 1.999 1.998)"));
        assertASTEquals("L[ true ]", eval("(<= 42 128)"));
        assertASTEquals("L[ true ]", eval("(<= 42 42)"));
    }
}