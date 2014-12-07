package de.mknblch.nolisp.features.array;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.interpreter.Dialect;

import java.util.HashMap;
import java.util.Map;

/**
 * @author mknblch
 */
public class ArrayDialect implements Dialect {

    private static HashMap<String, Object> features = new HashMap<>();

    static {
        features.put()
    }

    @Override
    public Map<String, Object> features() {
        return features;
    }
}
