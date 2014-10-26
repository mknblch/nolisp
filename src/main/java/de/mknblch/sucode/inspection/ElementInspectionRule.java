package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

/**
 * @author mknblch
 */
public interface ElementInspectionRule {

    public void inspect(ListStruct list);
}
