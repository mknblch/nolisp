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

    @Special
    @Define(symbol = "if")
    public static Object print(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        System.out.print(args.car());
        return null;
    }
}
