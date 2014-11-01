package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.forms.MacroForm;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.func.Special;
import de.mknblch.sucode.inspection.Inspector;
import de.mknblch.sucode.inspection.Rule;
import de.mknblch.sucode.inspection.RuleAdapter;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.parser.Parser;

import static de.mknblch.sucode.helper.Expectations.expectCdr;
import static de.mknblch.sucode.helper.TypeHelper.isList;
import static de.mknblch.sucode.helper.TypeHelper.symbolList;
import static de.mknblch.sucode.helper.TypeHelper.symbolLiteral;

/**
 * @author mknblch
 */
public class MacroForms {

    @Special
    @Define("defmacro") // (defmacro name (arg*) form+)
    public static Object defmacro(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        expectCdr(args);
        final Object symbol = args.car();
        final String name = symbolLiteral(symbol);
        parentContext.bind(name, new MacroForm(name, symbolList(args.cdar()), args.cddr()));
        return symbol;
    }

    @Special
    @Define("backquote")
    public static Object backquote(final Interpreter interpreter, final Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        if (!isList(car)) {
            return car;
        }
        final Rule replacementRule = new RuleAdapter() {
            @Override
            public void inspect(ListStruct container, Object element, int depth) throws Exception {
                // evaluate (comma <form>) structs only
                if (isList(element) && ((ListStruct) element).car() == Parser.COMMA_STRUCT) {
                    container.setCar(interpreter.eval(((ListStruct) element).cdar(), parentContext));
                }
            }
            @Override
            public boolean inspectSublists() {
                return true;
            }
        };
        Inspector.inspect((ListStruct) args, (Rule) replacementRule);
        return args.car(); //
    }

    @Special
    @Define("comma")
    public static Object comma(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        throw new EvaluationException("Bad syntax. Comma not inside a backquote environment.");
    }

    @Special
    @Define("splice")
    public static Object splice(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {

        final Object car = args.car();


        return null;
    }
}
