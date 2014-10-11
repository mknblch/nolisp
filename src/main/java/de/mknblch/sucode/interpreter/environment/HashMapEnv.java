package de.mknblch.sucode.interpreter.environment;

import java.util.*;

/**
 * Scoped map. Used to implement different variable scopes where a set operation<br/>
 * alters the local environment only and get operations pass from local to global<br/>
 * until the value is found.
 * <p/>
 * Created by mknblch on 10.10.2014.
 */
public class HashMapEnv implements Environment {

    private final HashMapEnv parentEnv;
    private final HashMap<String, Object> localMap;

    /**
     * construct empty environment.
     */
    public HashMapEnv() {
        this(null);
    }

    /**
     * used for derivation.
     */
    private HashMapEnv(HashMapEnv parentEnv) {
        this.parentEnv = parentEnv;
        this.localMap = new HashMap<String, Object>();
    }

    @Override
    public Environment derive() {
        return new HashMapEnv(this);
    }

    @Override
    public Environment getParentEnv() {
        return parentEnv;
    }

    @Override
    public void putGlobal(String key, Object value) {
        if (null != parentEnv) {
            parentEnv.putGlobal(key, value);
        } else {
            localMap.put(key, value);
        }
    }

    @Override
    public int size() {
        return localMap.size();
    }

    @Override
    public int sizeAll() {
        return keySetAll().size();
    }

    @Override
    public boolean isEmpty() {
        return localMap.isEmpty();
    }

    @Override
    public boolean isAllEmpty() {
        if (null == parentEnv) {
            return localMap.isEmpty();
        }
        return localMap.isEmpty() && parentEnv.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        if (null == parentEnv) {
            return localMap.containsKey(key);
        }
        return localMap.containsKey(key) || parentEnv.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    @Override
    public Object get(Object key) {
        final Object value = localMap.get(key);
        if (null != value) {
            return value;
        }
        return parentEnv != null ? parentEnv.get(key) : null;
    }

    @Override
    public Object put(String key, Object value) {
        return localMap.put(key, value);
    }

    @Override
    public Object remove(Object key) {
        return localMap.remove(key);
    }

    @Override
    public void removeEverywhere(String key) {
        localMap.remove(key);
        if (null == parentEnv) {
            return;
        }
        parentEnv.removeEverywhere(key);
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        for (Map.Entry<? extends String, ? extends Object> v : m.entrySet()) {
            put(v.getKey(), v.getValue());
        }
    }

    @Override
    public void clear() {
        localMap.clear();
    }

    @Override
    public void clearAll() {
        localMap.clear();
        if (null == parentEnv) {
            return;
        }
        parentEnv.clearAll();
    }

    @Override
    public Set<String> keySet() {
        return localMap.keySet();
    }

    @Override
    public Set<String> keySetAll() {
        if (null != parentEnv) {
            return union(keySet(), parentEnv.keySet());
        }
        return localMap.keySet();
    }

    @Override
    public Collection<Object> values() {
        return localMap.values();
    }

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
