package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.ast.ListStruct;

/**
 * Created by mknblch on 18.10.2014.
 */
public interface Interpreter {
    /**
     * evaluate obj in context.
     * <ul>
     * <li><b>null</b> evaluates to null</li>
     * <li><b>non Atom</b> evaluate to itself</li>
     * <li><b>SymbolStruct</b> evaluate to the object which is stored in context at Symbol.literal</li>
     * <li><b>ListStruct</b> is treated as function call which delegates the execution to the function
     * which must derive from evaluating the first argument of the list.</li>
     * </ul>
     */
    public Object eval(Object obj, Context context) throws Exception;

    /**
     * evaluates each element in the list without treating it as function call.
     */
    public ListStruct evalList(ListStruct list, Context context) throws Exception;

}
