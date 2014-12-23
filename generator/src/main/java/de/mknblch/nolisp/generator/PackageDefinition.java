package de.mknblch.nolisp.generator;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * @author mknblch
 */
public class PackageDefinition {

    private Map<String, DialectDefinition> map = new HashMap<>();

    public Collection<String> getPackages() {
        return map.keySet();
    }

    public Collection<DialectDefinition> getDialectDefinitions() {
        return map.values();
    }

    public DialectDefinition getOrCreate(String dialectName) {
        DialectDefinition definition = map.get(dialectName);
        if (null == definition) {
            definition = new DialectDefinition(dialectName);
            map.put(dialectName, definition);
        }
        return definition;
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        for (DialectDefinition dialectDefinition : map.values()) {
            builder.append(dialectDefinition);
        }
        return builder.toString();
    }
}
