package de.mknblch.nolisp.datatypes;

/**
 * @author mknblch
 */
public interface Form {

    public abstract Object eval(ListStruct args) throws Exception;
}
