package de.mknblch.nolisp.func;

/**
 * @author mknblch
 */
public class FunctionDefinitionException extends Exception {
    public FunctionDefinitionException() {
    }

    public FunctionDefinitionException(String message) {
        super(message);
    }

    public FunctionDefinitionException(String message, Throwable cause) {
        super(message, cause);
    }

    public FunctionDefinitionException(Throwable cause) {
        super(cause);
    }
}
