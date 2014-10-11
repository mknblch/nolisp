package de.mknblch.sucode.interpreter.forms;

/**
 * Created by mknblch on 11.10.2014.
 */
public class FormException extends Exception {
    public FormException() {
    }

    public FormException(String message) {
        super(message);
    }

    public FormException(String message, Throwable cause) {
        super(message, cause);
    }

    public FormException(Throwable cause) {
        super(cause);
    }
}
