package de.mknblch.nolisp.core.interpreter;

import de.mknblch.nolisp.core.scanner.FunctionDefinitionException;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author mknblch
 */
public class ContextTest {

    private static final Language NULL_LANG = new Language() {
        @Override
        public Map<String, Object> getContextMap() {
            return new HashMap<>();
        }
    };

    private static final Logger LOGGER = LoggerFactory.getLogger(ContextTest.class);

    public static Context makeEnv(String[] keys, Object[] values) throws FunctionDefinitionException {
        Assert.assertEquals("Erroneous test", keys.length, values.length);
        final Context env = new Context(NULL_LANG);
        for (int i = 0; i < keys.length; i++) {
            env.bind(keys[i], values[i]);
        }
        return env;
    }

    public static void dump(Context env) throws EvaluationException {
        do {
            LOGGER.debug("dumping Context ");
            for (String key : env.keySetLocal()) {
                LOGGER.debug("{} = {}", key, env.get(key));
            }
            env = env.getParent();
        } while (env != null);
    }

    @Test
    public void testDerive() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        Assert.assertNotNull(derived);
        Assert.assertTrue(derived.containsKey("a"));
        Assert.assertTrue(derived.containsKey("b"));
        Assert.assertTrue(derived.containsKey("c"));
        Assert.assertFalse(global.containsKey("c"));
    }

    @Test
    public void testPutGlobal() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bindGlobal("c", 3);
        dump(derived);

        Assert.assertNotNull(derived);
        Assert.assertTrue(derived.containsKey("a"));
        Assert.assertTrue(derived.containsKey("b"));
        Assert.assertTrue(derived.containsKey("c"));
        Assert.assertTrue(global.containsKey("c"));
    }

    @Test
    public void testSize() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        Assert.assertEquals(2, global.size());
        Assert.assertEquals(1, derived.size());
        Assert.assertEquals(3, derived.sizeGlobal());
    }

    @Test
    public void testIsEmpty() throws Exception {

        Context global = makeEnv(new String[]{}, new Object[]{});
        Context derived = global.derive();
        dump(derived);

        Assert.assertTrue(global.isEmpty());
        Assert.assertTrue(derived.isEmpty());
    }

    @Test
    public void testOneIsEmpty() throws Exception {

        Context global = makeEnv(new String[]{}, new Object[]{});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        Assert.assertTrue(global.isEmpty());
        Assert.assertFalse(derived.isEmpty());
    }

    @Test
    public void testIsNotEmpty() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        Assert.assertFalse(global.isEmpty());
        Assert.assertFalse(derived.isEmpty());
    }

    @Test
    public void testContainsKey() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        Assert.assertFalse(global.containsKey("c"));
        Assert.assertTrue(derived.containsKey("c"));
        Assert.assertTrue(derived.containsKey("a"));
        Assert.assertTrue(derived.containsKey("b"));
    }

    @Test
    public void testGet() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);
        dump(derived);

        Assert.assertNotNull(global.get("a"));
        Assert.assertNotNull(global.get("b"));
        Assert.assertFalse(global.containsKey("c"));

        Assert.assertNotNull(derived.get("a"));
        Assert.assertNotNull(derived.get("b"));
        Assert.assertNotNull(derived.get("c"));
    }

    @Test
    public void testBind() throws Exception {

        Context global = makeEnv(new String[]{}, new Object[]{});
        Context derived = derive(global);

        global.bind("a", 1);
        global.bind("b", 2);
        derived.bind("c", 3);
        dump(derived);

        Assert.assertFalse(global.containsKey("c"));
        Assert.assertNotNull(derived.get("c"));
        Assert.assertNotNull(global.get("a"));
        Assert.assertNotNull(global.get("b"));
    }

    @Test
    public void testRemove() throws Exception {

        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derived = derive(global);
        derived.bind("c", 3);

        derived.unbind("c");
        derived.unbind("b");
        derived.unbind("a");

        dump(derived);
        Assert.assertTrue(derived.isEmpty());
    }

    @Test
    public void testBindAll() throws Exception {
        Context e1 = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});

        HashMap<String, Object> e2 = new HashMap<String, Object>() {{
            put("b", 3);
            put("c", 4);
        }};

        e1.bindAll(e2);
        Assert.assertEquals(3, e1.sizeGlobal());
    }

    @Test
    public void testKeySetLocal() throws Exception {
        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Set<String> keys = global.keySetLocal();
        Assert.assertTrue(keys.contains("a"));
        Assert.assertTrue(keys.contains("b"));
    }

    @Test
    public void testKeySetGlobal() throws Exception {
        Context global = makeEnv(new String[]{"a", "b"}, new Object[]{1, 2});
        Context derive = global.derive();
        derive.bind("c", 3);
        Set<String> keys = derive.keySetGlobal();
        Assert.assertTrue(keys.contains("a"));
        Assert.assertTrue(keys.contains("b"));
        Assert.assertTrue(keys.contains("c"));
    }

    private Context derive(Context global) {
        return (Context) global.derive();
    }
}
