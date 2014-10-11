package de.mknblch.sucode.evaluator;

import java.util.*;

/**
 * chained map. used to implement different variable scopes where a set operation
 * alters the local environment only and get operations pass from local to global.
 * <p/>
 * Created by mknblch on 10.10.2014.
 */
public class Environment<K, V> implements Map<K, V> {

    private final Environment<K, V> globalEnv;
    private final HashMap<K, V> localMap;

    /**
     * construct empty environment.
     */
    public Environment() {
        this(null);
    }

    /**
     * used for derivation.
     */
    private Environment(Environment<K, V> globalEnv) {
        this.globalEnv = globalEnv;
        this.localMap = new HashMap<K, V>();
    }

    /**
     * derive a new local environment (new scope) with this as it's parent.
     * @return
     */
    public Environment<K, V> derive() {
        return new Environment<K, V>(this);
    }

    /**
     * retrieve parent environment
     * @return
     */
    public Environment<K, V> getParentEnv() {
        return globalEnv;
    }

    /**
     * put value in the most significant env if any. put to local env otherwise.
     */
    public void putGlobal(K key, V value) {
        if (null != globalEnv) {
            globalEnv.putGlobal(key, value);
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
        if (null == globalEnv) {
            return localMap.isEmpty();
        }
        return localMap.isEmpty() && globalEnv.isEmpty();
    }

    /**
     * check if local or global env contains the key.
     */
    @Override
    public boolean containsKey(Object key) {
        if (null == globalEnv) {
            return localMap.containsKey(key);
        }
        return localMap.containsKey(key) || globalEnv.containsKey(key);
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
    public V get(Object key) {
        final V value = localMap.get(key);
        if (null != value) {
            return value;
        }
        return globalEnv != null ? globalEnv.get(key) : null;
    }

    /**
     * put value into local environment.
     */
    @Override
    public V put(K key, V value) {
        return localMap.put(key, value);
    }

    /**
     * remove element from local env only.
     */
    @Override
    public V remove(Object key) {
        return localMap.remove(key);
    }

    /**
     * remove key.value pair from local and all parent environments.
     * @param key
     * @return always null
     */
    public V removeEverywhere(Object key) {
        localMap.remove(key);
        if (null == globalEnv) {
            return null;
        }
        globalEnv.removeEverywhere(key);
        return null;
    }

    /**
     * put all into local env.
     */
    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        for (Entry<? extends K, ? extends V> v : m.entrySet()) {
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
        if (null == globalEnv) {
            return;
        }
        globalEnv.clearAll();
    }

    /**
     * get local keySet
     */
    @Override
    public Set<K> keySet() {
        return localMap.keySet();
    }

    /**
     * get union from local and all global keySets.
     */
    public Set<K> keySetAll() {
        if (null != globalEnv) {
            final Set<K> localSet = keySet();
            final Set<K> globalSet = globalEnv.keySet();
            final Set<K> union = new HashSet<K>(localSet.size() + globalSet.size());
            union.addAll(localSet);
            union.addAll(globalSet);
            return union;
        }
        return localMap.keySet();
    }

    /**
     * return local value collection.
     */
    @Override
    public Collection<V> values() {
        return localMap.values();
    }

    /**
     * return local entry set.
     */
    @Override
    public Set<Entry<K, V>> entrySet() {
        return localMap.entrySet();
    }
}
