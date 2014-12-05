package de.mknblch.nolisp.inspection;

import de.mknblch.nolisp.datatypes.ListStruct;

/**
 * @author mknblch
 */
public interface InspectionRule {

    /**
     * called upon inspection of each element in the list.
     * the actual behaviour can be controlled with inspectSublists() which tells the inspector to omit calling and
     * to continues with the next concrete element or with follow(..) which can skip sublists completely.
     *
     * @param container the container listStruct of element
     * @param element   the actual element ( container.car )
     * @param depth
     */
    public void inspect(ListStruct container, Object element, int depth) throws Exception;

    /**
     * tells if inspect(..) should be called whenever the head element is a ListStruct.
     *
     * @return true if SubLists should be inspected, false otherwise.
     */
    public boolean inspectSublists();

    /**
     * tells whether the inspector should dive into the listElement or
     * omit it and continue with next element in the container list.
     *
     * @param container   parent container of the listElement.
     * @param listElement element of the list which itself is a ListStruct.
     * @param depth
     * @return true if inspector should dive into listElement, false otherwise.
     */
    public boolean follow(ListStruct container, ListStruct listElement, int depth);
}
