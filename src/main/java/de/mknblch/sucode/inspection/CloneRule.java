package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

/**
 * @author mknblch
 */
public interface CloneRule {

    public Object clone (Object element) throws Exception;

//    public boolean follow (ListStruct listElement);

}
