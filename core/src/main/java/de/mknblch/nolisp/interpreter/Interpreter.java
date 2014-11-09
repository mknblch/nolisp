package de.mknblch.nolisp.interpreter;

import de.mknblch.nolisp.ast.ListStruct;

/**
 * @author mknblch
 */
public interface Interpreter {
    /**
     * evaluate obj in context.
     * <ul>
     * <li><b>null</b> evaluates to null</li>
     * <li><b>non Atom</b> evaluate to itself</li>
     * <li><b>SymbolStruct</b> evaluate to the object which is stored in context at Symbol.literal</li>
     * <li><b>ListStruct</b> is treated as function call which delegates the execution to the function
     * which derives by evaluating the first argument of the list.</li>
     * </ul>
     */
    public Object eval(Object obj, Context context) throws Exception;

    /**
     * evaluates each element in the list without treating it as function call.
     */
    public ListStruct evalEach(ListStruct list, Context context) throws Exception;

}