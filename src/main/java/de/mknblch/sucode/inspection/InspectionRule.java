package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

/**
 * @author mknblch
 */
public interface InspectionRule {

    public void inspect(ListStruct list);

    public void inspectList(ListStruct list);
}
