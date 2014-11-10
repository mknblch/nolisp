package de.mknblch.nolisp.minimallisp.forms;

import de.mknblch.nolisp.annotations.Define;
import de.mknblch.nolisp.annotations.Special;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.ast.forms.MacroForm;
import de.mknblch.nolisp.helper.Expectations;
import de.mknblch.nolisp.helper.TypeHelper;
import de.mknblch.nolisp.inspection.CloneRule;
import de.mknblch.nolisp.inspection.Inspector;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
public class MacroForms {

    @Special
    @Define("defmacro") // (defmacro name (arg*) form+)
    public static Object defmacro(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object symbol = args.car();
        parentContext.bind(TypeHelper.symbolLiteral(symbol), new MacroForm(TypeHelper.symbolList(args.cdar()), args.cddr()));
        return symbol;
    }

    @Special
    @Define("backquote")
    public static Object backquote(final Interpreter interpreter, final Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        if (!TypeHelper.isList(car)) {
            return car;
        }

        final CloneRule cloneRule = new CloneRule() {
            @Override
            public Object clone(Object element) throws Exception {
                if (TypeHelper.isList(element) && TypeHelper.isSymbolWithLiteral(((ListStruct) element).car(), "comma")) { // == Parser.COMMA_STRUCT) {
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
