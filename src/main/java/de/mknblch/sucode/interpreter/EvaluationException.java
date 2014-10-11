package de.mknblch.sucode.interpreter;

/**
 * Created by mknblch on 10.10.2014.
 */
public class EvaluationException extends Exception {
    public EvaluationException() { }

    public EvaluationException(String message) {
        super(message);
    }

    public EvaluationException(String message, Throwable cause) {
        super(message, cause);
    }

    public EvaluationException(Throwable cause) {
        super(cause);
    }
}
