package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.func.FunctionBuilder;
import de.mknblch.sucode.func.FunctionDefinitionException;

import java.util.*;

/**
 * Scoped map. Used to implement different variable scopes where a set() operation<br/>
 * alters only the local environment and get() operations pass from local to global<br/>
 * environment until the key is found.
 * <p/>
 * @author mknblch
 */
public class Context {

    private final Context parentEnv;
    private final HashMap<String, Object> localMap;

    /**
     * construct empty environment.
     */
    public Context() {
        this.parentEnv = null;
        this.localMap = new HashMap<String, Object>();
    }
    public Context(Class<?> ...buildInFunctionContainer) throws FunctionDefinitionException {
        this();
        defineAll(FunctionBuilder.build(buildInFunctionContainer));
    }

    /**
     * used for derivation.
     */
    public Context(Context parentEnv) {
        this.parentEnv = parentEnv;
        this.localMap = new HashMap<String, Object>();
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
    public Context getParentEnv() {
        return parentEnv;
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
        throw new EvaluationException(String.format("Reference to undefined identifier: '%s'.", key));
    }

    /**
     * unbindLocal element from local env only.
     */
    public Object unbindLocal(Object key) {
        return localMap.remove(key);
    }

    /**
     * global operation. unbindLocal key-value pair from local and all parent environments.
     */
    public void unbind(String key) {
        localMap.remove(key);
        if (null == parentEnv) {
            return;
        }
        parentEnv.unbind(key);
    }

    /**
     * local operation.
     */
    public void define (Function function) {
        bind(function.getSymbol(), function);
    }

    public void defineAll (Collection<Function> functions) {
        for (Function function : functions) {
            define(function);
        }
    }

    public void defineGlobal (Function function) {
        bindGlobal(function.getSymbol(), function);
    }

    public void defineAllGlobal (Collection<Function> functions) {
        for (Function function : functions) {
            defineGlobal(function);
        }
    }


    public void undefineLocal(String key) {
        unbindLocal(key);
    }

    /**
     * global operation
     */
    public void undefine(String key) {
        unbind(key);
    }


    /**
     * put value into local environment.
     */
    public void bind(String key, Object value) {
        localMap.put(key, value);
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
     * put all into local env.
     */
    public void bindAll(Map<String, Object> m) {
        for (Map.Entry<String, Object> v : m.entrySet()) {
            bind(v.getKey(), v.getValue());
        }
    }

    /**
     * put all into local env.
     */
    public void bindAllGlobal(Map<String, Object> m) {
        for (Map.Entry<String, Object> v : m.entrySet()) {
            bindGlobal(v.getKey(), v.getValue());
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

    private static <U> Set<U> union(Set<U> a, Set<U> globalSet) {
        final Set<U> union = new HashSet<U>(a.size() + globalSet.size());
        union.addAll(a);
        union.addAll(globalSet);
        return union;
    }
}
