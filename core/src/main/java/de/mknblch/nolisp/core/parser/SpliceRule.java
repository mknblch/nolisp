package de.mknblch.nolisp.core.parser;

import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.inspection.ContainerCloneRule;
import de.mknblch.nolisp.core.datatypes.ListStruct;

/**
 * This rule splices a list into it's parent list. <br/>
 * (1 @(2 @(3 4) 5) 6) => (1 2 3 4 5 6)
 *
 * @author mknblch
 */
public class SpliceRule implements ContainerCloneRule {

    @Override
    public ListStruct cloneSublist(ListStruct container) throws Exception {

        if (TypeHelper.isSymbolWithLiteral(container.car(), "splice")) {
            final ListStruct listStruct = TypeHelper.asList(container.cadr());
            final ListStruct clone = new ListStruct();
            //noinspection ConstantConditions
            for (Object o : listStruct) {
                clone.add(o);
            }
            return clone;
        }

        return new ListStruct(container);
    }

    @Override
    public ListStruct cloneElement(Object element) throws Exception {
        return new ListStruct(element);
    }
}