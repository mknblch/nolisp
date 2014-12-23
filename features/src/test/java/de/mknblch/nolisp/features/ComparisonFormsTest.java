package de.mknblch.nolisp.features;

import de.mknblch.nolisp.testHelper.AbstractFormTest;
import org.junit.Test;

public class ComparisonFormsTest extends AbstractFormTest {

    @Test
    public void testEqual() throws Exception {

        AbstractFormTest.assertASTEquals("L[ true ]", eval("(== 2 2)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(== 1.999 1.999)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(== 2.0 2)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(== 1 1.0)"));
    }
    @Test
    public void testGreater() throws Exception {

        AbstractFormTest.assertASTEquals("L[ true ]", eval("(> 2 1.999)"));
        AbstractFormTest.assertASTEquals("L[ false ]", eval("(> 1 1.999)"));
    }

    @Test
    public void testGreaterEqual() throws Exception {

        AbstractFormTest.assertASTEquals("L[ true ]", eval("(>= 1.999 1.999)"));
        AbstractFormTest.assertASTEquals("L[ false ]", eval("(>= 1.998 1.999)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(>= 42 2)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(>= 42 42)"));
    }

    @Test
    public void testLower() throws Exception {

        AbstractFormTest.assertASTEquals("L[ false ]", eval("(< 2 1.9999)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(< 1 1.0001)"));
    }

    @Test
    public void testLowerEqual() throws Exception {

        AbstractFormTest.assertASTEquals("L[ true ]", eval("(<= 1.999 1.999)"));
        AbstractFormTest.assertASTEquals("L[ false ]", eval("(<= 1.999 1.998)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(<= 42 128)"));
        AbstractFormTest.assertASTEquals("L[ true ]", eval("(<= 42 42)"));
    }
}