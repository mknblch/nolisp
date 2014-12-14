package de.mknblch.nolisp.dialect;

import de.mknblch.nolisp.interpreter.Dialect;

import java.util.HashMap;
import java.util.Map;

/**
 * @author mknblch
 */
public class EmptyDialect implements Dialect {

    protected HashMap<String, Object> features = new HashMap<>();
    private final String name;

    public EmptyDialect(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Map<String, Object> features() {
        return features;
    }




}
