package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;
import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.LambdaForm;
import de.mknblch.nolisp.core.interpreter.structs.forms.MacroForm;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * @author mknblch
 */
public class PredicateForms {

    @Special
    @Define({"=", "==", "eq", "eq?", "equal?"}) // (eq? 1 3)
    public static Object equal(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object a = interpreter.eval(args.car(), context);
        final Object b = args.cdr() != null ? interpreter.eval(args.cdr().car(), context) : null;
        return equal(a, b);
    }

    public static Object equal(Object a, Object b) {
        if (null == a && null == b) return true;
        if (null == a) return false;
        if (null == b) return false;
        return a.equals(b);
    }

    @Define("null?")
    public static Object isNull(Context context, ListStruct args) {
        return null == args.car();
    }

    @Define("int?")
    public static Object isInt(Context context, ListStruct args) {
        return args.car() instanceof Integer;
    }

    @Define("real?")
    public static Object isReal(Context context, ListStruct args) {
        return args.car() instanceof Double;
    }

    @Define("string?")
    public static Object isString(Context context, ListStruct args) {
        return args.car() instanceof String;
    }

    @Special
    @Define("value?")
    public static Object isSymbol(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof SymbolStruct;
    }

    @Special
    @Define("list?")
    public static Object isList(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof ListStruct;
    }

    @Special
    @Define("lambda?")
    public static Object isLambda(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof LambdaForm;
    }

    @Special
    @Define("macro?")
    public static Object isMacro(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof MacroForm;
    }

    @Special
    @Define("atom?")
    public static Object isAtom(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof Atom;
    }
}
