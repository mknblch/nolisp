package de.mknblch.nolisp.interpreter;

import java.util.Map;

/**
 * @author mknblch
 */
public interface Dialect {

    public Map<String, Object> features();
}
