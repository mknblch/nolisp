package de.mknblch.sucode.interpreter.forms;

import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.parser.structs.ListStruct;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

/**
 * Created by mknblch on 11.10.2014.
 */
public class FormRegisterTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(FormRegisterTest.class);

    @Test
    public void testRegister() throws Exception {

        FormRegister formRegister = new FormRegister();

        formRegister.register(FormRegisterTest.class);

        dump(formRegister);
    }

    public static void dump(FormRegister formRegister) {
        Set<String> strings = formRegister.keySet();
        for (String string : strings) {
            LOGGER.debug(String.format("Found form '%s'", string));

        }
    }

    @Function
    public static Object sum (ListStruct args, Environment env, Interpreter interpreter) {
        return 0;
    }

    @Function(symbol = {"foo", "bar"})
    public static Boolean prim (ListStruct args, Environment env, Interpreter interpreter) {
        return false;
    }
}
