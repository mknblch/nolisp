package de.mknblch.nolisp.parser;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.inspection.CloneRule;
import de.mknblch.nolisp.datatypes.ListStruct;

/**
 * This rule splices a list into it's parent list. <br/>
 * (1 @(2 @(3 4) 5) 6) => (1 2 3 4 5 6)
 *
 * @author mknblch
 */
public class SpliceRule implements CloneRule {

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
