package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

/**
 * @author mknblch
 */
public class RuleAdapter implements Rule {

    @Override
    public void inspect(ListStruct container, Object element, int depth) throws Exception {}

    @Override
    public boolean inspectSublists() {
        return false;
    }

    @Override
    public boolean follow(ListStruct container, ListStruct listElement, int depth) {
        return true;
    }
}
