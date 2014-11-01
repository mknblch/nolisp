package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.interpreter.EvaluationException;

/**
 * @author mknblch
 */
public interface Rule {

    public void startAt(Object element);

    public void inspect(ListStruct container, Object element);

    public boolean follow(ListStruct list);
}
