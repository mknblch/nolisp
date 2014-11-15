package de.mknblch.nolisp.core.interpreter;

import java.util.Map;

/**
 * @author mknblch
 */
public interface Language {

    public String getName();

    public String getVersion();

    public Map<String, Object> getConstants();

    public Map<String, Object> getFunctions();

    public void include(Language language);
}