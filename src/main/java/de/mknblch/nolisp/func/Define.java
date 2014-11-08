package de.mknblch.nolisp.func;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author mknblch
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Define {
    String[] value();

    /**
     * <p>returns whether the function is a special form or not. </p>
     *
     * <p>special forms take their arguments as pure ast and evaluate them under certain conditions.
     * to do so a reference to the actual interpreter is needed and must be defined as static field using
     * the @{@link Special} annotation.<br>
     * </p>
     *
     * <p>if not the arguments will be evaluated BEFORE function call. therefore no
     * @{@link Special} is needed.</p>
     */
}
