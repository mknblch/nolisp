package de.mknblch.nolisp.interpreter;

import de.mknblch.nolisp.annotations.FunctionDefinitionException;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Scoped map. Used to implement different variable scopes where a set() operation<br/>
 * alters only the local environment and get() operations pass from local to global<br/>
 * environment until the key is found.
 * <p/>
 * @author mknblch
 */
public class Context {

    private final Context parent;
    private final HashMap<String, Object> map;
    private final boolean global;

    /**
     * construct empty global environment.
     */
    public Context() { // TODO remove ?
        global = true;
        this.parent = null;
        this.map = new HashMap<>();
    }

    public Context(Language language) throws FunctionDefinitionException {
        this();
        bindAll(language.getConstants());
        bindAll(language.getFunctions());
        bind("_lang", language.getName());
        bind("_version", language.getVersion());
    }

    /**
     * used for derivation.
     */
    private Context(Context parent) {
        global = false;
        this.parent = parent;
        this.map = new HashMap<>();
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
     * the element will be retrieved from global environments. if no global
     * env is specified, null is returned.
     */
    public Object get(Object key) throws EvaluationException {
        if(map.containsKey(key)) {
            return map.get(key);
        }
        if(null != parent && parent.containsKey(key)) {
            return parent.get(key);
        }
        throw new EvaluationException(String.format("Reference to undefined identifier: '%s'.", key));
    }

    /**
     * unbindLocal element from local env only.
     */
    public Object unbind(Object key) {
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
            return union(keySetLocal(), parent.keySetLocal());
        }
        return map.keySet();
    }

    private static <U> Set<U> union(Set<U> a, Set<U> globalSet) {
        final Set<U> union = new HashSet<>(a.size() + globalSet.size());
        union.addAll(a);
        union.addAll(globalSet);
        return union;
    }
}
