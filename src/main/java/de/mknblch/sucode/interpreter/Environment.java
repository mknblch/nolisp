package de.mknblch.sucode.interpreter;

import java.util.*;

/**
 * Scoped map. Used to implement different variable scopes where a set operation<br/>
 * alters the local environment only and get operations pass from local to global<br/>
 * until the value is found.
 * <p/>
 * Created by mknblch on 10.10.2014.
 */
public class Environment implements Map<String, Object> {

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
         * @return
         */
    public Environment derive() {
        return new Environment(this);
    }

    /**
         * retrieve parent environment
         * @return
         */
    public Environment getParentEnv() {
        return parentEnv;
    }

    /**
         * put value in the most significant env if any. put to local env otherwise.
         */
    public void putGlobal(String key, Object value) {
        if (null != parentEnv) {
            parentEnv.putGlobal(key, value);
        } else {
            localMap.put(key, value);
        }
    }

    /**
         * retrieve size of local map.
         */
    @Override
    public int size() {
        return localMap.size();
    }

    /**
         * returns the size of this + parent environments.
         * expensive operation because the keySet union of the
         * local and all global environments must be calculated.
         */
    public int sizeAll() {
        return keySetAll().size();
    }

    /**
         * check if local env is empty.
         */
    @Override
    public boolean isEmpty() {
        return localMap.isEmpty();
    }

    /**
         * decides whether the env (including it's parents) is empty.
         */
    public boolean isAllEmpty() {
        if (null == parentEnv) {
            return localMap.isEmpty();
        }
        return localMap.isEmpty() && parentEnv.isEmpty();
    }

    /**
         * check if local or global env contains the key.
         */
    @Override
    public boolean containsKey(Object key) {
        if (null == parentEnv) {
            return localMap.containsKey(key);
        }
        return localMap.containsKey(key) || parentEnv.containsKey(key);
    }

    /**
         * not implemented because it's not decidable if the
         * maps contain different values for the same key.
         */
    @Override
    public boolean containsValue(Object value) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    /**
         * get value. if the key is found in local map it's value is used. if not
         * the element will be retrieved from global environments. if no global
         * env is specified, null is returned.
         */
    @Override
    public Object get(Object key) {
        final Object value = localMap.get(key);
        if (null != value) {
            return value;
        }
        return parentEnv != null ? parentEnv.get(key) : null;
    }

    /**
         * put value into local environment.
         */
    @Override
    public Object put(String key, Object value) {
        return localMap.put(key, value);
    }

    /**
         * remove element from local env only.
         */
    @Override
    public Object remove(Object key) {
        return localMap.remove(key);
    }

    /**
         * remove key.value pair from local and all parent environments.
         * @param key
         */
    public void removeEverywhere(String key) {
        localMap.remove(key);
        if (null == parentEnv) {
            return;
        }
        parentEnv.removeEverywhere(key);
    }

    /**
         * put all into local env.
         */
    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        for (Map.Entry<? extends String, ? extends Object> v : m.entrySet()) {
            put(v.getKey(), v.getValue());
        }
    }

    /**
         * clear local environment.
         */
    @Override
    public void clear() {
        localMap.clear();
    }

    /**
         * clear local and all global envs.
          */
    public void clearAll() {
        localMap.clear();
        if (null == parentEnv) {
            return;
        }
        parentEnv.clearAll();
    }

    /**
         * get local keySet
         */
    @Override
    public Set<String> keySet() {
        return localMap.keySet();
    }

    /**
         * get union from local and all global keySets.
         */
    public Set<String> keySetAll() {
        if (null != parentEnv) {
            return union(keySet(), parentEnv.keySet());
        }
        return localMap.keySet();
    }

    /**
         * return local value collection.
         */
    @Override
    public Collection<Object> values() {
        return localMap.values();
    }

    /**
         * return local entry set.
         */
    @Override
    public Set<Map.Entry<String, Object>> entrySet() {
        return localMap.entrySet();
    }

    private static <U> Set<U> union(Set<U> localSet, Set<U> globalSet) {
        final Set<U> union = new HashSet<U>(localSet.size() + globalSet.size());
        union.addAll(localSet);
        union.addAll(globalSet);
        return union;
    }
}
