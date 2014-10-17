package de.mknblch.sucode.func;

/**
 * Created by mknblch on 12.10.2014.
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
