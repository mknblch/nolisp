package de.mknblch.nolisp.inspection;

import de.mknblch.nolisp.ast.ListStruct;

/**
 * @author mknblch
 */
public interface ListRule {

    /**
     * called upon inspection of each element in the list.
     * @param container the container listStruct of element
     * @param element the actual element ( container.car )
     * @return whether the inspector should continue iterating the list
     */
    public boolean inspect(ListStruct container, Object element) throws Exception;

}
