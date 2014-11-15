package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.MacroForm;
import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.Converter;
import de.mknblch.nolisp.core.inspection.CloneRule;
import de.mknblch.nolisp.core.inspection.Inspector;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.Context;

/**
 * @author mknblch
 */
public class MacroForms {

    @Special
    @Define("defmacro") // (defmacro name (arg*) form+)
    public static Object defmacro(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object symbol = args.car();
        parentContext.bind(Converter.symbolLiteral(symbol), new MacroForm(Converter.symbolList(args.cdar()), args.cddr()));
        return symbol;
    }

    @Special
    @Define("backquote")
    public static Object backquote(final Interpreter interpreter, final Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        if (!Converter.isList(car)) {
            return car;
        }

        final CloneRule cloneRule = new CloneRule() {
            @Override
            public Object clone(Object element) throws Exception {
                if (Converter.isList(element) && Converter.isSymbolWithLiteral(((ListStruct) element).car(), "comma")) { // == Parser.COMMA_STRUCT) {
                    return interpreter.eval(((ListStruct) element).cdar(), parentContext);
                }
                return element;
            }
        };

        return Inspector.cloneTree(args, cloneRule).car();
    }

    @Special
    @Define("comma")
    public static Object comma(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        throw new EvaluationException("Bad syntax. Comma not inside a backquote environment.");
    }

    @Special
    @Define("splice")
    public static Object splice(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        // TODO implement
        throw new EvaluationException("Bad syntax. @ not inside a backquote environment.");
    }
}
