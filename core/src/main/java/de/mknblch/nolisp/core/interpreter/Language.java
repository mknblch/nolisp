package de.mknblch.nolisp.core.interpreter;

import de.mknblch.nolisp.core.scanner.FunctionDefinitionException;

import java.util.Map;

/**
 * @author mknblch
 */
public interface Language {

    public Context makeContext () throws Exception;

}
