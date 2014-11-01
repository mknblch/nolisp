package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.interpreter.EvaluationException;

/**
 * @author mknblch
 */
public class RuleAdapter implements Rule {

    @Override
    public void inspect(ListStruct container, Object element) throws Exception {}

    @Override
    public boolean inspectSublists() {
        return false;
    }

    @Override
    public boolean follow(ListStruct container, ListStruct listElement) {
        return true;
    }
}
