package de.mknblch.sucode.evaluator;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.*;

/**
 * Created by mknblch on 11.10.2014.
 */
public class EnvironmentTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(EnvironmentTest.class);

    public static Environment<String, Object> makeEnv(String[] keys, Object[] values) {
        assertEquals("Erroneous test", keys.length, values.length);
        final Environment<String, Object> env = new Environment<String, Object>();
        for (int i = 0; i < keys.length; i++) {
            env.put(keys[i], values[i]);
        }
        return env;
    }

    public static void dump(Environment<String, Object> env) {
        do {
            LOGGER.debug("dumping {}", env);
            for(String key : env.keySet()) {
                LOGGER.debug("{} = {}", key, env.get(key));
            }
            env = env.getParentEnv();
        } while (env != null);
    }

    @Test
    public void testDerive() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        dump(derived);

        assertNotNull(derived);
        assertTrue(derived.containsKey("a"));
        assertTrue(derived.containsKey("b"));
        assertTrue(derived.containsKey("c"));
        assertFalse(global.containsKey("c"));
    }

    @Test
    public void testPutGlobal() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.putGlobal("c", 3);
        dump(derived);

        assertNotNull(derived);
        assertTrue(derived.containsKey("a"));
        assertTrue(derived.containsKey("b"));
        assertTrue(derived.containsKey("c"));
        assertTrue(global.containsKey("c"));
    }

    @Test
    public void testSize() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        dump(derived);

        assertEquals(2, global.size());
        assertEquals(1, derived.size());
        assertEquals(3, derived.sizeAll());
    }

    @Test
    public void testIsEmpty() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{}, new Object[]{});
        Environment<String, Object> derived = global.derive();
        dump(derived);

        assertTrue(global.isEmpty());
        assertTrue(derived.isEmpty());
    }

    @Test
    public void testOneIsEmpty() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{}, new Object[]{});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        dump(derived);

        assertTrue(global.isEmpty());
        assertFalse(derived.isEmpty());
    }

    @Test
    public void testIsNotEmpty() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        dump(derived);

        assertFalse(global.isEmpty());
        assertFalse(derived.isEmpty());
    }

    @Test
    public void testContainsKey() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        dump(derived);

        assertFalse(global.containsKey("c"));
        assertTrue(derived.containsKey("c"));
        assertTrue(derived.containsKey("a"));
        assertTrue(derived.containsKey("b"));
    }

    @Test
    public void testGet() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        dump(derived);

        assertNotNull(global.get("a"));
        assertNotNull(global.get("b"));
        assertNull(global.get("c"));

        assertNotNull(derived.get("a"));
        assertNotNull(derived.get("b"));
        assertNotNull(derived.get("c"));
    }

    @Test
    public void testPut() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{}, new Object[]{});
        Environment<String, Object> derived = global.derive();

        global.put("a", 1);
        global.put("b", 2);
        derived.put("c", 3);
        dump(derived);

        assertNull(global.get("c"));
        assertNotNull(derived.get("c"));
        assertNotNull(global.get("a"));
        assertNotNull(global.get("b"));
    }

    @Test
    public void testRemove() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);

        derived.remove("c");
        derived.remove("b");
        derived.remove("a");

        dump(derived);
        assertTrue(derived.isEmpty());
        assertFalse(derived.isAllEmpty());
        assertFalse(global.isEmpty());
    }

    @Test
    public void testRemoveEverywhere() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);

        derived.removeEverywhere("c");
        derived.removeEverywhere("b");
        derived.removeEverywhere("a");

        dump(derived);
        assertTrue(derived.isAllEmpty());
    }

    @Test
    public void testPutAll() throws Exception {
        Environment<String, Object> e1 = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> e2 = makeEnv(new String[]{"c", "b"}, new Object[]{3, 2});
        e1.putAll(e2);
        assertEquals(3, e1.sizeAll());
    }

    @Test
    public void testClearLocal() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        derived.clear();
        assertEquals(0, derived.size());
        assertEquals(2, global.size());
    }

    @Test
    public void testClearParent() throws Exception {

        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        global.clear();
        assertEquals(1, derived.size());
        assertEquals(0, global.size());
    }

    @Test
    public void testClearAll() throws Exception {
        Environment<String, Object> global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Environment<String, Object> derived = global.derive();
        derived.put("c", 3);
        derived.clearAll();
        assertEquals(0, derived.sizeAll());
    }

    @Test
    public void testKeySet() throws Exception {

    }
}
