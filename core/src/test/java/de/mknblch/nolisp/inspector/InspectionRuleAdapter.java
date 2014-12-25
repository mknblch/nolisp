package de.mknblch.nolisp.inspector;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.inspection.InspectionRule;

/**
 * @author mknblch
 */
public class InspectionRuleAdapter implements InspectionRule {

    @Override
    public void inspect(ListStruct container, Object element, int depth) throws Exception {
    }

    @Override
    public boolean inspectSublists() {
        return true;
    }

    @Override
    public boolean follow(ListStruct container, ListStruct listElement, int depth) {
        return true;
    }
}
