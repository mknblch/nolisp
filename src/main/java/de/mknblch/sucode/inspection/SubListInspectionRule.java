package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

/**
 * @author mknblch
 */
public interface SubListInspectionRule {

    public void inspect(ListStruct list);

    public boolean inspectSubList(ListStruct list);
}
