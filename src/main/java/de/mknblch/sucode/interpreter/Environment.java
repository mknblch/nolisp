package de.mknblch.sucode.interpreter;

import java.util.*;

/**
 * Scoped map. Used to implement different variable scopes where a set() operation<br/>
 * alters only the local environment and get() operations pass from local to global<br/>
 * environment until the key is found.
 * <p/>
 * Created by mknblch on 10.10.2014.
 */
public class Environment {

    private final Environment parentEnv;
    private final HashMap<String, Object> localMap;

    /**
     * construct empty environment.
     */
    public Environment() {
        this(null);
    }

    /**
     * used for derivation.
     */
    private Environment(Environment parentEnv) {
        this.parentEnv = parentEnv;
        this.localMap = new HashMap<String, Object>();
    }

    /**
     * derive a new local environment (new scope) with this as it's parent.
     */
    public Environment derive() {
        return new Environment(this);
    }

    /**
     * retrieve parent environment
     */
    public Environment getParentEnv() {
        return parentEnv;
    }

    /**
     * put value in the most significant env if any. put to local env otherwise.
     */
    public void bindGlobal(String key, Object value) {
        if (null != parentEnv) {
            parentEnv.bindGlobal(key, value);
        } else {
            localMap.put(key, value);
        }
    }

    /**
     * retrieve size of local map.
     */
    public int size() {
        return localMap.size();
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
    public boolean isEmptyLocal() {
        return localMap.isEmpty();
    }

    /**
         * decides whether the env (including it's parents) is empty.
         */
    public boolean isEmptyGlobal() {
        if (null == parentEnv) {
            return localMap.isEmpty();
        }
        return localMap.isEmpty() && parentEnv.isEmptyLocal();
    }

    /**
     * check if local or global env contains the key.
     */
    public boolean containsKey(Object key) {
        if (null == parentEnv) {
            return localMap.containsKey(key);
        }
        return localMap.containsKey(key) || parentEnv.containsKey(key);
    }

    /**
     * get value. if the key is found in local map it's value is used. if not
     * the element will be retrieved from global environments. if no global
     * env is specified, null is returned.
     */
    public Object get(Object key) throws EvaluationException {

        if(localMap.containsKey(key)) {
            return localMap.get(key);
        }
        if(null != parentEnv && parentEnv.containsKey(key)) {
            return parentEnv.get(key);
        }

        throw new EvaluationException(String.format("Unbound variable '%s'.", key));
    }

    /**
     * removeLocal element from local env only.
     */
    public Object removeLocal(Object key) {
        return localMap.remove(key);
    }

    /**
     * removeLocal key.value pair from local and all parent environments.
     */
    public void removeGlobal(String key) {
        localMap.remove(key);
        if (null == parentEnv) {
            return;
        }
        parentEnv.removeGlobal(key);
    }

    /**
     * put value into local environment.
     */
    public void bind(String key, Object value) {
        localMap.put(key, value);
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
     * clearLocal local environment.
     */
    public void clearLocal() {
        localMap.clear();
    }

    /**
     * clearLocal local and global env.
      */
    public void clearGlobal() {
        localMap.clear();
        if (null == parentEnv) {
            return;
        }
        parentEnv.clearGlobal();
    }

    /**
         * get local keySetLocal
         */
    public Set<String> keySetLocal() {
        return localMap.keySet();
    }

    /**
         * get union from local and all global keySets.
         */
    public Set<String> keySetGlobal() {
        if (null != parentEnv) {
            return union(keySetLocal(), parentEnv.keySetLocal());
        }
        return localMap.keySet();
    }

    /**
     * return local entry set.
     */
    public Set<Map.Entry<String, Object>> entrySet() {
        return localMap.entrySet();
    }

    private static <U> Set<U> union(Set<U> a, Set<U> globalSet) {
        final Set<U> union = new HashSet<U>(a.size() + globalSet.size());
        union.addAll(a);
        union.addAll(globalSet);
        return union;
    }
}
