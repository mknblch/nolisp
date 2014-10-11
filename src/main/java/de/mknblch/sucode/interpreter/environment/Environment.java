package de.mknblch.sucode.interpreter.environment;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * Scoped map. Used to implement different variable scopes where a set operation<br/>
 * alters the local environment only and get operations pass from local to global<br/>
 * until the value is found.
 *
 * Created by mknblch on 11.10.2014.
 */
public interface Environment extends Map<String, Object> {
    /**
     * derive a new local environment (new scope) with this as it's parent.
     * @return
     */
    Environment derive();

    /**
     * retrieve parent environment
     * @return
     */
    Environment getParentEnv();

    /**
     * put value in the most significant env if any. put to local env otherwise.
     */
    void putGlobal(String key, Object value);

    /**
     * retrieve size of local map.
     */
    int size();

    /**
     * returns the size of this + parent environments.
     * expensive operation because the keySet union of the
     * local and all global environments must be calculated.
     */
    int sizeAll();

    /**
     * check if local env is empty.
     */
    boolean isEmpty();

    /**
     * decides whether the env (including it's parents) is empty.
     */
    boolean isAllEmpty();

    /**
     * check if local or global env contains the key.
     */
    boolean containsKey(Object key);

    /**
     * not implemented because it's not decidable if the
     * maps contain different values for the same key.
     */
    boolean containsValue(Object value);

    /**
     * get value. if the key is found in local map it's value is used. if not
     * the element will be retrieved from global environments. if no global
     * env is specified, null is returned.
     */
    Object get(Object key);

    /**
     * put value into local environment.
     */
    Object put(String key, Object value);

    /**
     * remove element from local env only.
     */
    Object remove(Object key);

    /**
     * remove key.value pair from local and all parent environments.
     * @param key
     */
    void removeEverywhere(String key);

    /**
     * put all into local env.
     */
    void putAll(Map<? extends String, ? extends Object> m);

    /**
     * clear local environment.
     */
    void clear();

    /**
     * clear local and all global envs.
      */
    void clearAll();

    /**
     * get local keySet
     */
    Set<String> keySet();

    /**
     * get union from local and all global keySets.
     */
    Set<String> keySetAll();

    /**
     * return local value collection.
     */
    Collection<Object> values();

    /**
     * return local entry set.
     */
    Set<Map.Entry<String, Object>> entrySet();
}
