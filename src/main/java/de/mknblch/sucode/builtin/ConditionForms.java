package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.func.Special;
import de.mknblch.sucode.helper.TypeHelper;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

/**
 * Created by mknblch on 12.10.2014.
 */
public class ConditionForms {

    /*
        (if condition yes no)
     */
    @Special
    @Define(symbol = "if")
    public static Object ifCondition(Interpreter interpreter, Context context, ListStruct args) throws Exception {

        final boolean condition = TypeHelper.asBoolean(interpreter.eval(args.car(), context));

        final Object trueBranch = args.cdr().car();
        final ListStruct cddr = args.cdr().cdr();
        final Object falseBranch = cddr.car();

        if(condition) return interpreter.eval(trueBranch, context);
        else return interpreter.eval(falseBranch, context);
    }

    /*
        null?	    Tell if the argument is nil (empty list).
        pair?	    Tell if the argument is a pair (result of cons, most often a non-empty list.)
        id?	        Tells if the argument is an identifier.
        int?	    Tells if the argument is an integer.
        str?	    Tells if the argument is a string.
        builtin?	Tells if the argument is a builtin function.
        lambda?	    Tells if the argument is a function (result of the lambda operator).
        macro?	    Tells if the argument is a macro (result of the macro operator).
        functional?	Tells if the argument is something that can be called with a parameter list as a function is. These are any of the last three types.
     */
}
