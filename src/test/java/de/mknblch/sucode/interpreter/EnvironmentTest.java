package de.mknblch.sucode.interpreter;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Set;

import static org.junit.Assert.*;

/**
 * Created by mknblch on 11.10.2014.
 */
public class EnvironmentTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(EnvironmentTest.class);

    public static Environment makeEnv(String[] keys, Object[] values) {
        assertEquals("Erroneous test", keys.length, values.length);
        final Environment env = new Environment();
        for (int i = 0; i < keys.length; i++) {
            env.bind(keys[i], values[i]);
        }
        return env;
    }

    public static void dump(Environment env) throws EvaluationException {
        do {
            LOGGER.debug("dumping {}", env);
            for(String key : env.keySetLocal()) {
                LOGGER.debug("{} = {}", key, env.get(key));
            }
            env = env.getParentEnv();
        } while (env != null);
    }

    @Test
    public void testDerive() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertNotNull(derived);
        assertTrue(derived.containsKey("a"));
        assertTrue(derived.containsKey("b"));
        assertTrue(derived.containsKey("c"));
        assertFalse(global.containsKey("c"));
    }

    @Test
    public void testPutGlobal() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bindGlobal("c", 3);
        dump(derived);

        assertNotNull(derived);
        assertTrue(derived.containsKey("a"));
        assertTrue(derived.containsKey("b"));
        assertTrue(derived.containsKey("c"));
        assertTrue(global.containsKey("c"));
    }

    @Test
    public void testSize() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertEquals(2, global.size());
        assertEquals(1, derived.size());
        assertEquals(3, derived.sizeGlobal());
    }

    @Test
    public void testIsEmpty() throws Exception {

        Environment global = makeEnv(new String[]{}, new Object[]{});
        Environment derived = global.derive();
        dump(derived);

        assertTrue(global.isEmptyLocal());
        assertTrue(derived.isEmptyLocal());
    }

    @Test
    public void testOneIsEmpty() throws Exception {

        Environment global = makeEnv(new String[]{}, new Object[]{});
        Environment derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertTrue(global.isEmptyLocal());
        assertFalse(derived.isEmptyLocal());
    }

    @Test
    public void testIsNotEmpty() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertFalse(global.isEmptyLocal());
        assertFalse(derived.isEmptyLocal());
    }

    @Test
    public void testContainsKey() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertFalse(global.containsKey("c"));
        assertTrue(derived.containsKey("c"));
        assertTrue(derived.containsKey("a"));
        assertTrue(derived.containsKey("b"));
    }

    @Test
    public void testGet() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertNotNull(global.get("a"));
        assertNotNull(global.get("b"));
        assertFalse(global.containsKey("c"));

        assertNotNull(derived.get("a"));
        assertNotNull(derived.get("b"));
        assertNotNull(derived.get("c"));
    }

    @Test
    public void testBind() throws Exception {

        Environment global = makeEnv(new String[]{}, new Object[]{});
        Environment derived = derive(global);

        global.bind("a", 1);
        global.bind("b", 2);
        derived.bind("c", 3);
        dump(derived);

        assertFalse(global.containsKey("c"));
        assertNotNull(derived.get("c"));
        assertNotNull(global.get("a"));
        assertNotNull(global.get("b"));
    }

    @Test
    public void testRemove() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);

        derived.removeLocal("c");
        derived.removeLocal("b");
        derived.removeLocal("a");

        dump(derived);
        assertTrue(derived.isEmptyLocal());
        assertFalse(derived.isEmptyGlobal());
        assertFalse(global.isEmptyLocal());
    }

    @Test
    public void testRemoveGlobal() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);

        derived.removeGlobal("c");
        derived.removeGlobal("b");
        derived.removeGlobal("a");

        dump(derived);
        assertTrue(derived.isEmptyGlobal());
    }

    @Test
    public void testBindAll() throws Exception {
        Environment e1 = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});

        HashMap<String, Object> e2 = new HashMap<String, Object>() {{
            put("b", 3);
            put("c", 4);
        }};

        e1.bindAll(e2);
        assertEquals(3, e1.sizeGlobal());
    }

    @Test
    public void testClearLocal() throws Exception {

        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);
        derived.clearLocal();
        assertEquals(0, derived.size());
        assertEquals(2, global.size());
    }

    @Test
    public void testClearGlobal() throws Exception {
        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derived = derive(global);
        derived.bind("c", 3);
        derived.clearGlobal();
        assertEquals(0, derived.sizeGlobal());
    }

    @Test
    public void testKeySetLocal() throws Exception {
        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Set<String> keys = global.keySetLocal();
        assertTrue(keys.contains("a"));
        assertTrue(keys.contains("b"));
    }

    @Test
    public void testKeySetGlobal() throws Exception {
        Environment global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment derive = global.derive();
        derive.bind("c", 3);
        Set<String> keys = derive.keySetGlobal();
        assertTrue(keys.contains("a"));
        assertTrue(keys.contains("b"));
        assertTrue(keys.contains("c"));
    }

    private Environment derive(Environment global) {
        return (Environment) global.derive();
    }
}
