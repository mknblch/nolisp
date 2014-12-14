package de.mknblch.nolisp.scanner;

import de.mknblch.nolisp.interpreter.Dialect;

import java.util.HashMap;
import java.util.Map;

/**
 * @author mknblch
 */
public class EmptyDialect implements Dialect {

    private HashMap<String, Object> features = new HashMap<>();

    @Override
    public Map<String, Object> features() {
        return features;
    }

}
