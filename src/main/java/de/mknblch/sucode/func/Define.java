package de.mknblch.sucode.func;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Created by mknblch on 11.10.2014.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Define {
    String[] symbol();

    /**
     * <p>returns whether the function is a special form or not. </p>
     *
     * <p>special forms take their arguments as pure structs and evaluate them under certain conditions.
     * to do so a reference to the actual interpreter is needed and must be defined as static field using
     * the @{@link Special} annotation.<br>
     * </p>
     *
     * <p>if not the arguments will be evaluated BEFORE function call. therefore no
     * @{@link Special} is needed.</p>
     */
//    boolean special() default false;
}
