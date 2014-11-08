package de.mknblch.nolisp.inspection;

import de.mknblch.nolisp.ast.ListStruct;

/**
 * @author mknblch
 */
public class TreeRuleAdapter implements TreeRule {

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
