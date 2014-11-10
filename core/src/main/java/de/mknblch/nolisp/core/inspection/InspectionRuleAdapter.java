package de.mknblch.nolisp.core.inspection;

import de.mknblch.nolisp.core.ast.ListStruct;

/**
 * @author mknblch
 */
public class InspectionRuleAdapter implements InspectionRule {

    @Override
    public void inspect(ListStruct container, Object element, int depth) throws Exception {}

    @Override
    public boolean inspectSublists() {
        return true;
    }

    @Override
    public boolean follow(ListStruct container, ListStruct listElement, int depth) {
        return true;
    }
}
