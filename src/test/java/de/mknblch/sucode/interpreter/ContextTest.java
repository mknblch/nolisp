package de.mknblch.sucode.interpreter;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Set;

import static org.junit.Assert.*;

/**
 * @author mknblch
 */
public class ContextTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ContextTest.class);

    public static Context makeEnv(String[] keys, Object[] values) {
        assertEquals("Erroneous test", keys.length, values.length);
        final Context env = new Context();
        for (int i = 0; i < keys.length; i++) {
            env.bind(keys[i], values[i]);
        }
        return env;
    }

    public static void dump(Context env) throws EvaluationException {
        do {
            LOGGER.debug("dumping Context ");
            for(String key : env.keySetLocal()) {
                LOGGER.debug("{} = {}", key, env.get(key));
            }
            env = env.getParentEnv();
        } while (env != null);
    }

    @Test
    public void testDerive() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
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

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
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

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertEquals(2, global.size());
        assertEquals(1, derived.size());
        assertEquals(3, derived.sizeGlobal());
    }

    @Test
    public void testIsEmpty() throws Exception {

        Context global = makeEnv(new String[]{}, new Object[]{});
        Context derived = global.derive();
        dump(derived);

        assertTrue(global.isEmptyLocal());
        assertTrue(derived.isEmptyLocal());
    }

    @Test
    public void testOneIsEmpty() throws Exception {

        Context global = makeEnv(new String[]{}, new Object[]{});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertTrue(global.isEmptyLocal());
        assertFalse(derived.isEmptyLocal());
    }

    @Test
    public void testIsNotEmpty() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertFalse(global.isEmptyLocal());
        assertFalse(derived.isEmptyLocal());
    }

    @Test
    public void testContainsKey() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        assertFalse(global.containsKey("c"));
        assertTrue(derived.containsKey("c"));
        assertTrue(derived.containsKey("a"));
        assertTrue(derived.containsKey("b"));
    }

    @Test
    public void testGet() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
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

        Context global = makeEnv(new String[]{}, new Object[]{});
        Context derived = derive(global);

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

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);

        derived.unbindLocal("c");
        derived.unbindLocal("b");
        derived.unbindLocal("a");

        dump(derived);
        assertTrue(derived.isEmptyLocal());
        assertFalse(derived.isEmptyGlobal());
        assertFalse(global.isEmptyLocal());
    }

    @Test
    public void testRemoveGlobal() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);

        derived.unbind("c");
        derived.unbind("b");
        derived.unbind("a");

        dump(derived);
        assertTrue(derived.isEmptyGlobal());
    }

    @Test
    public void testBindAll() throws Exception {
        Context e1 = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});

        HashMap<String, Object> e2 = new HashMap<String, Object>() {{
            put("b", 3);
            put("c", 4);
        }};

        e1.bindAll(e2);
        assertEquals(3, e1.sizeGlobal());
    }

    @Test
    public void testClearLocal() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        derived.clearLocal();
        assertEquals(0, derived.size());
        assertEquals(2, global.size());
    }

    @Test
    public void testClearGlobal() throws Exception {
        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        derived.clearGlobal();
        assertEquals(0, derived.sizeGlobal());
    }

    @Test
    public void testKeySetLocal() throws Exception {
        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Set<String> keys = global.keySetLocal();
        assertTrue(keys.contains("a"));
        assertTrue(keys.contains("b"));
    }

    @Test
    public void testKeySetGlobal() throws Exception {
        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derive = global.derive();
        derive.bind("c", 3);
        Set<String> keys = derive.keySetGlobal();
        assertTrue(keys.contains("a"));
        assertTrue(keys.contains("b"));
        assertTrue(keys.contains("c"));
    }

    private Context derive(Context global) {
        return (Context) global.derive();
    }
}
