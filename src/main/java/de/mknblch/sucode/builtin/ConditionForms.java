package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.Atom;
import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.func.Special;
import de.mknblch.sucode.helper.TypeHelper;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class ConditionForms {

    /*
        (if condition yes no)
     */
    @Special
    @Define(value = "if")
    public static Object ifImpl(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final boolean condition = TypeHelper.asBoolean(interpreter.eval(args.car(), context));
        final Object trueBranch = args.cdar();
        final Object falseBranch = args.cddar();
        if(condition) return interpreter.eval(trueBranch, context);
        else return interpreter.eval(falseBranch, context);
    }

    @Special
    @Define(value = "cond") // (cond () () ())
    public static Object cond(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        for (Object arg : args) {
            TypeHelper.expectList(arg);
            // (bool form)
            final ListStruct pair = (ListStruct) arg;
            final Object condition = interpreter.eval(pair.car(), context);
            if (TypeHelper.asBoolean(condition)) {
                return interpreter.eval(pair.cdar(), context);
            }
        }
        return null;
    }

    @Special
    @Define(value = {"=", "==", "eq", "eq?", "equal?"}) // (eq? 1 3)
    public static Object equal(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object a = interpreter.eval(args.car(), context);
        final Object b = args.cdr() != null ? interpreter.eval(args.cdr().car(), context) : null;
        if(null == a) {
            if (null == b) return true;
            else return false;
        }
        if(null == b) {
            if (null == a) return true;
            else return false;
        }
        return a.equals(b);
    }

    @Define(value = "null?")
    public static Object isNull(Context context, ListStruct args) {
        return null == args.car();
    }

    @Define(value = "int?")
    public static Object isInt(Context context, ListStruct args) {
        return args.car() instanceof Integer;
    }

    @Define(value = "real?")
    public static Object isReal(Context context, ListStruct args) {
        return args.car() instanceof Double;
    }

    @Define(value = "string?")
    public static Object isString(Context context, ListStruct args) {
        return args.car() instanceof String;
    }

    @Special
    @Define(value = "value?")
    public static Object isSymbol(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof SymbolStruct;
    }

    @Special
    @Define(value = "list?")
    public static Object isList(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof ListStruct;
    }

    @Special
    @Define(value = "lambda?")
    public static Object isLambda(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof LambdaForm;
    }

    @Special
    @Define(value = {"function?", "functional?"})
    public static Object isFunctional(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof Function;
    }

    @Special
    @Define(value = {"atom?", "atomar?"})
    public static Object isAtom(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof Atom;
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
