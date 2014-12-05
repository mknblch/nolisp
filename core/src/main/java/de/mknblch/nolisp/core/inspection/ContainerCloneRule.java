package de.mknblch.nolisp.core.inspection;

import de.mknblch.nolisp.core.datatypes.ListStruct;

/**
 * @author mknblch
 */
public interface ContainerCloneRule {

    public ListStruct cloneSublist(ListStruct container) throws Exception;

    public ListStruct cloneElement(Object element) throws Exception;
}
