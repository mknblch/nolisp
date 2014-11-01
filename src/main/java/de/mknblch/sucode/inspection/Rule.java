package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.interpreter.EvaluationException;

/**
 * @author mknblch
 */
public interface Rule {

    /**
     * called upon inspection of each element in the list.
     * the actual behaviour can be controlled with inspectSublists() which tells the inspector to omit calling and
     * to continues with the next concrete element or with follow(..) which can skip sublists completely.
     * @param container the container listStruct of element
     * @param element the actual element ( container.car )
     */
    public void inspect(ListStruct container, Object element) throws Exception;

    /**
     * tells if inspect(..) should be called whenever the head element is a ListStruct.
     * @return true if SubLists should be inspected, false otherwise.
     */
    public boolean inspectSublists();

    /**
     * tells whether the inspector should dive into the listElement or
     * omit it and continue with next element in the container list.
     * @param container parent container of the listElement.
     * @param listElement element of the list which itself is a ListStruct.
     * @return true if inspector should dive into listElement, false otherwise.
     */
    public boolean follow(ListStruct container, ListStruct listElement);
}
