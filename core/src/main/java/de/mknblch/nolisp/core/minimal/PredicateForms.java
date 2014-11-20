package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.Atom;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.LambdaForm;
import de.mknblch.nolisp.core.interpreter.structs.forms.MacroForm;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

/**
 * @author mknblch
 */
public class PredicateForms {

    @Define({"eq?", "equal?"}) // (eq? 1 3)
    public static Object equal(ListStruct args) throws Exception {
        return equal(args.car(), args.cdar());
    }

    private static Object equal(Object a, Object b) {
        if (null == a && null == b) return true;
        return null != a && null != b && a.equals(b);
    }

    @Define("null?")
    public static Object isNull(ListStruct args) {
        return null == args.car();
    }

    @Special
    @Define("instanceof?") // (instanceof? java.lang.String "bla") => true
    public static Object isInstance(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String className = TypeHelper.symbolLiteral(args.car());
        final Object cdar = args.cdar();
        Expectations.expectNotNull(cdar);
        final Object value = interpreter.eval(cdar, context);
        return className.equals(value.getClass().getName());
    }

    @Define("int?")
    public static Object isInt(ListStruct args) {
        return args.car() instanceof Integer;
    }

    @Define("real?")
    public static Object isReal(ListStruct args) {
        return args.car() instanceof Double;
    }

    @Define("string?")
    public static Object isString(ListStruct args) {
        return args.car() instanceof String;
    }

    @Special
    @Define("symbol?")
    public static Object isSymbol(Interpreter interpreter, Context context, ListStruct args) {
        return args.car() instanceof SymbolStruct;
    }

    @Define("list?")
    public static Object isList(ListStruct args) {
        return args.car() instanceof ListStruct;
    }

    @Define("lambda?")
    public static Object isLambda( ListStruct args) {
        return args.car() instanceof LambdaForm;
    }

    @Define("macro?")
    public static Object isMacro(ListStruct args) {
        return args.car() instanceof MacroForm;
    }

    @Define("atom?")
    public static Object isAtom(ListStruct args) {
        return args.car() instanceof Atom;
    }
}
