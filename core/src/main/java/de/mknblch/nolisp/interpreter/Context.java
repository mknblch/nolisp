package de.mknblch.nolisp.interpreter;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Scoped map. Used to implement different variable scopes where a set() operation<br/>
 * alters only the local environment and get() operations pass from local to global<br/>
 * environment until the key is found.
 * <p/>
 *
 * @author mknblch
 */
public class Context {

    private final Context parent;
    private final HashMap<String, Object> map = new HashMap<>();
    private final boolean global;

    /**
     * make empty context
     */
    public Context() {
        this.global = true;
        this.parent = null;
    }

    /**
     * used for derivation.
     */
    private Context(Context parent) {
        this.global = false;
        this.parent = parent;
    }

    /**
     * derive a new local environment (new scope) with this as it's parent.
     */
    public Context derive() {
        return new Context(this);
    }

    /**
     * retrieve parent environment
     */
    public Context getParent() {
        return parent;
    }

    /**
     * retrieve size of local map.
     */
    public int size() {
        return map.size();
    }

    /**
     * returns the size of this + parent environments.
     * expensive operation because the keySetLocal union of the
     * local and all global environments must be calculated.
     */
    public int sizeGlobal() {
        return keySetGlobal().size();
    }

    /**
     * check if local env is empty.
     */
    public boolean isEmpty() {
        return map.isEmpty();
    }

    /**
     * check if local or global env contains the key.
     */
    public boolean containsKey(Object key) {
        if (global) {
            return map.containsKey(key);
        }
        return map.containsKey(key) || parent.containsKey(key);
    }

    /**
     * get value. if the key is found in local map it's value is used. if not
     * the element will be retrieved from global environments.
     */
    public Object get(Object key) throws EvaluationException {
        if (map.containsKey(key)) {
            return map.get(key);
        }
        if (null != parent && parent.containsKey(key)) {
            return parent.get(key);
        }
        throw new EvaluationException(String.format("Reference to undefined identifier: '%s'.", key));
    }

    /**
     * unbind element.
     */
    public Object unbind(Object key) {
        if (null != parent) {
            parent.unbind(key);
        }
        return map.remove(key);
    }

    /**
     * put value into local environment.
     */
    public void bind(String key, Object value) {
        map.put(key, value);
    }

    /**
     * put value in the most significant env if any. put to local env otherwise.
     */
    public void bindGlobal(String key, Object value) {
        if (null != parent) {
            parent.bindGlobal(key, value);
        } else {
            map.put(key, value);
        }
    }


    /**
     * overwrite value in the context where it was defined. throws ex otherwise.
     */
    public void bindToContainer(String key, Object value) throws EvaluationException {
        if(map.containsKey(key)) {
            map.put(key, value);
        } else if(null == parent) {
            throw new EvaluationException(String.format("Reference to undefined identifier: '%s'.", key));
        } else {
            parent.bindToContainer(key, value);
        }
    }

    /**
     * put all into local env.
     */
    public void bindAll(Map<String, Object> m) {
        for (Map.Entry<String, Object> v : m.entrySet()) {
            bind(v.getKey(), v.getValue());
        }
    }

    /**
     * get local keySetLocal
     */
    public Set<String> keySetLocal() {
        return map.keySet();
    }

    /**
     * get union from local and all global keySets.
     */
    public Set<String> keySetGlobal() {
        if (null != parent) {
            return union(keySetLocal(), parent.keySetGlobal());
        }
        return map.keySet();
    }

    public void implement (Dialect... features) {
        for (Dialect dialect : features) {
            bindAll(dialect.features());
        }
    }

    private static <U> Set<U> union(Set<U> a, Set<U> b) {
        final Set<U> union = new HashSet<>(a.size() + b.size());
        union.addAll(a);
        union.addAll(b);
        return union;
    }
}
