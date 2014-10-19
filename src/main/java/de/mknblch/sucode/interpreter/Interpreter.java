package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.ast.ListStruct;

/**
 * Created by mknblch on 18.10.2014.
 */
public interface Interpreter {

    public Object eval(Object obj, Context context) throws Exception;

    /**
     * evaluates each element in the list without treating the outer list as function call.
     */
    public ListStruct evalList(ListStruct list, Context context) throws Exception;

}
