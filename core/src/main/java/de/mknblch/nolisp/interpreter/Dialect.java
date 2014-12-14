package de.mknblch.nolisp.interpreter;

import java.util.Map;

/**
 * @author mknblch
 */
public interface Dialect {

    public String getName();

    public Map<String, Object> features();
}
