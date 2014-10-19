package de.mknblch.sucode.builtin;

import de.mknblch.sucode.func.AbstractFormTest;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 19.10.2014.
 */
public class ConditionFormsTest extends AbstractFormTest {
    @Test
    public void testIf() throws Exception {

        final List<Object> result = eval("(if nil 43 42)");
        assertEquals(42, result.get(0));
    }
}
