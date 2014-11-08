package de.mknblch.nolisp.builtin;

import de.mknblch.nolisp.ast.Atom;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.ast.SymbolStruct;
import de.mknblch.nolisp.ast.forms.Function;
import de.mknblch.nolisp.ast.forms.LambdaForm;
import de.mknblch.nolisp.func.Define;
import de.mknblch.nolisp.func.Special;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class PredicateForms {

    @Special
    @Define(value = {"=", "==", "eq", "eq?", "equal?"}) // (eq? 1 3)
    public static Object equal(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object a = interpreter.eval(args.car(), context);
        final Object b = args.cdr() != null ? interpreter.eval(args.cdr().car(), context) : null;
        return equal(a, b);
    }

    public static Object equal(Object a, Object b) {
        if (null == a && null == b) return true;
        if(null == a) return false;
        if(null == b) return false;
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
}
