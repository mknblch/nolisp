package de.mknblch.sucode.interpreter.forms;

import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.parser.structs.ListStruct;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

import static junit.framework.Assert.assertEquals;
import static junit.framework.TestCase.assertTrue;

/**
 * Created by mknblch on 11.10.2014.
 */
public class FormRegisterTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(FormRegisterTest.class);

    @Test(expected = FormException.class)
    public void shouldFail_WrongSignature() throws Exception {
        new FormRegister().register(WrongSignature.class);
    }

    @Test(expected = FormException.class)
    public void shouldFail_NoReturnType() throws Exception {
        new FormRegister().register(NoReturnType.class);
    }

    @Test(expected = FormException.class)
    public void shouldFail_NotStatic() throws Exception {
        new FormRegister().register(NotStatic.class);
    }

    @Test(expected = FormException.class)
    public void shouldFail_functionRedefinition() throws Exception {
        final FormRegister formRegister = new FormRegister();
        formRegister.register(Working.class);
        formRegister.register(Working.class);
    }

    @Test
    public void shouldWork_registerValid() throws Exception {

        final FormRegister formRegister = new FormRegister();
        formRegister.register(Working.class);
        dump(formRegister);
        assertEquals(4, formRegister.size());
        assertTrue(formRegister.containsForm("sum"));
        assertTrue(formRegister.containsForm("foo"));
        assertTrue(formRegister.containsForm("bar"));
        assertTrue(formRegister.containsForm("baz"));
    }

    public static void dump(FormRegister formRegister) {
        Set<String> strings = formRegister.keySet();
        for (String string : strings) {
            LOGGER.debug(String.format("Found form '%s'", string));
        }
    }
}
