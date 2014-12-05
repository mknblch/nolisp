package de.mknblch.nolisp.inspection;

import de.mknblch.nolisp.datatypes.ListStruct;

/**
 * @author mknblch
 */
public interface ContainerCloneRule {

    public ListStruct cloneSublist(ListStruct container) throws Exception;

    public ListStruct cloneElement(Object element) throws Exception;
}
