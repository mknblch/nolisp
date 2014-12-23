package de.mknblch.nolisp.generator;

import java.util.*;

/**
 * @author mknblch
 */
public class DialectDefinition {

    public final String packageName; // a.b.c.Class
    private final String className;
    private final Map<String, String> functions = new HashMap<>();
    private final Map<String, String> constants = new HashMap<>();
    private final Set<String> packages = new HashSet<>();

    public DialectDefinition(String packageName) {
        this.packageName = packageName;
        this.className = extractTargetClassName(packageName);
    }

    public void addFunction(String symbol, String className) {
        functions.put(symbol, className);
        packages.add(className);
    }

    public void addConstant(String symbol, String className) {
        constants.put(symbol, className);
        packages.add(className.substring(0, className.lastIndexOf('.')));
    }

    public Set<String> getPackages() {
        return packages;
    }

    public String getPackageName() {
        return packageName;
    }

    public String getClassName() {
        return className;
    }

    public Map<String, String> getFunctions() {
        return functions;
    }

    public Map<String, String> getConstants() {
        return constants;
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append(packageName.toUpperCase()).append(" { \n");
        for (Map.Entry<String, String> entry : constants.entrySet()) {
            builder.append(entry.getKey()).append("=").append(entry.getValue()).append("\n");
        }
        for (Map.Entry<String, String> entry : functions.entrySet()) {
            builder.append(entry.getKey()).append("=").append(entry.getValue()).append("\n");
        }
        builder.append("}");
        return builder.toString();
    }

    private static String extractTargetClassName(String name) {
        final int beginIndex = name.lastIndexOf('.');
        if (beginIndex == -1) {
            return name;
        }
        final String className = name.substring(beginIndex+1);
        return className.substring(0,1).toUpperCase() + className.substring(1);
    }
}
