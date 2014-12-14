package de.mknblch.nolisp.dialect;

import org.atteo.classindex.IndexAnnotated;
import org.atteo.classindex.IndexSubclasses;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author mknblch
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Constant {
    String[] value();
}
