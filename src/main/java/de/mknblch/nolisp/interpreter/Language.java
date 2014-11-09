package de.mknblch.nolisp.interpreter;

import java.util.Map;

/**
 * @author mknblch
 */
public interface Language {

    public String getName();

    public String getVersion();

    public Map<String, Object> getConstants();

    public Map<String, Object> getFunctions();
}
