package de.mknblch.sucode.evaluator;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * chained map.
 * <p/>
 * Created by mknblch on 10.10.2014.
 */
public class Environment<K, V> implements Map<K, V> {

    private final HashMap<K, V> localMap;

    private final Environment<K, V> globalEnv;

    public Environment() {
        this(null);
    }

    public Environment(Environment<K, V> globalEnv) {
        this.globalEnv = globalEnv;
        this.localMap = new HashMap<K, V>();
    }

    /**
     * put value in the most significant map
     */
    public void putGlobal(K key, V value) {
        if (null == globalEnv) {
            localMap.put(key, value);
        }
        globalEnv.putGlobal(key, value);
    }

    @Override
    public int size() {
        if (null == globalEnv) {
            return localMap.size();
        }
        return localMap.size() + globalEnv.size();
    }

    @Override
    public boolean isEmpty() {
        if (null == globalEnv) {
            return localMap.isEmpty();
        }
        return localMap.isEmpty() && globalEnv.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        if (null == globalEnv) {
            return localMap.containsKey(key);
        }
        return localMap.containsKey(key) && globalEnv.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        if (null == globalEnv) {
            return localMap.containsValue(value);
        }
        return localMap.containsValue(value) && globalEnv.containsValue(value);
    }

    @Override
    public V get(Object key) {
        final V value = localMap.get(key);
        if (null != value) {
            return value;
        }
        return globalEnv.get(key);
    }

    @Override
    public V put(K key, V value) {
        return localMap.put(key, value);
    }

    @Override
    public V remove(Object key) {
        return localMap.remove(key);
    }

    public V removeGlobal(Object key) {
        globalEnv.removeGlobal(key);
        localMap.remove(key);
        return null;
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        for (Entry<? extends K, ? extends V> v : m.entrySet()) {
            put(v.getKey(), v.getValue());
        }
    }

    @Override
    public void clear() {
        localMap.clear();
    }

    public void clearGlobal() {
        localMap.clear();
        globalEnv.clearGlobal();
    }

    @Override
    public Set<K> keySet() {
        if (null != globalEnv) {
            final Set<K> kSet = localMap.keySet();
            kSet.addAll(globalEnv.keySet());
            return kSet;
        }
        return localMap.keySet();
    }

    @Override
    public Collection<V> values() {
        throw new RuntimeException("Not implemented.");
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        throw new RuntimeException("Not implemented.");
    }
}
