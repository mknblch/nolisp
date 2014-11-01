package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.interpreter.EvaluationException;

/**
 * @author mknblch
 */
public class RuleAdapter implements Rule {

    @Override
    public void startAt(Object element) {}

    @Override
    public void inspect(ListStruct container, Object element) {}

    @Override
    public boolean follow(ListStruct list) {
        return true;
    }
}
