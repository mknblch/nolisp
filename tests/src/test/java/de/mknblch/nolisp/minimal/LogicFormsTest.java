package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.minimal.testHelper.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

public class LogicFormsTest extends AbstractFormTest {

    @Test
    public void testAnd() throws Exception {

        assertASTEquals("L[ true ]", eval("(and true true)"));
        assertASTEquals("L[ false ]", eval("(and true false)"));
        assertASTEquals("L[ false ]", eval("(and false true)"));
    }

    @Test
    public void testMultipleAnd() throws Exception {

        assertASTEquals("L[ true ]", eval("(and true true true true true)"));
        assertASTEquals("L[ false ]", eval("(and true true true true false true)"));
    }

    @Test
    public void testOr() throws Exception {

        assertASTEquals("L[ true ]", eval("(or true true)"));
        assertASTEquals("L[ true ]", eval("(or true false)"));
        assertASTEquals("L[ true ]", eval("(or false true)"));
        assertASTEquals("L[ false ]", eval("(or false false)"));
    }

    @Test
    public void testMultipleOr() throws Exception {

        assertASTEquals("L[ true ]", eval("(or false false false true)"));
        assertASTEquals("L[ false ]", eval("(or false false false)"));
    }

    @Test
    public void testXor() throws Exception {

        assertASTEquals("L[ false ]", eval("(xor true true)"));
        assertASTEquals("L[ false ]", eval("(xor false false)"));
        assertASTEquals("L[ true ]", eval("(xor true false)"));
        assertASTEquals("L[ true ]", eval("(xor false true)"));
    }

    @Test
    public void testNot() throws Exception {

        assertASTEquals("L[ false ]", eval("(not true)"));
        assertASTEquals("L[ true ]", eval("(not false)"));
    }
}