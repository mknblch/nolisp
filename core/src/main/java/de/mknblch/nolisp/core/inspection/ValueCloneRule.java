package de.mknblch.nolisp.core.inspection;

/**
 * @author mknblch
 */
public interface ValueCloneRule {

    /**
     * this method must return the clone of element which itself may be
     * a list
     *
     * @param element the source
     * @return the clone
     * @throws Exception
     */
    public Object clone(Object element) throws Exception;

}
