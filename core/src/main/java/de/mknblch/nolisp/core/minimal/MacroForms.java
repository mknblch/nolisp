package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.inspection.ContainerCloneRule;
import de.mknblch.nolisp.core.inspection.Inspector;
import de.mknblch.nolisp.core.inspection.ValueCloneRule;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.MacroForm;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

/**
 * @author mknblch
 */
public class MacroForms {

    @Special
    @Define("defmacro") // (defmacro name (arg*) form+)
    public static Object defmacro(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object symbol = args.car();
        parentContext.bind(TypeHelper.getSymbolLiteral(symbol), new MacroForm(TypeHelper.convertToSymbolList(args.cadr()), args.cddr()));
        return symbol;
    }

    @Special
    @Define("backquote")
    public static Object backquote(final Interpreter interpreter, final Context context, ListStruct args) throws Exception {
        final Object car = args.car();
        if (!TypeHelper.isList(car)) {
            return car;
        }

        final ContainerCloneRule rule = new ContainerCloneRule() {

            @Override
            public ListStruct cloneSublist(ListStruct container) throws Exception {

                if (TypeHelper.isSymbolWithLiteral(container.car(), "comma-splice")) {
                    final ListStruct listStruct = TypeHelper.asList(interpreter.eval(container.cadr(), context));
                    final ListStruct clone = new ListStruct();
                    //noinspection ConstantConditions
                    for (Object o : listStruct) {
                        clone.add(o);
                    }
                    return clone;
                }

                else if (TypeHelper.isSymbolWithLiteral(container.car(), "comma")) {
                    return new ListStruct(interpreter.eval(container.cadr(), context));
                }

                return new ListStruct(container);
            }

            @Override
            public ListStruct cloneElement(Object element) throws Exception {
                return new ListStruct(element);
            }
        };

        return Inspector.cloneTree(args, rule).car();
    }

    @Special
    @Define("comma")
    public static Object comma(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        throw new EvaluationException("Bad syntax. Comma not inside a backquote environment.");
    }

    @Special
    @Define("splice")
    public static Object splice(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        throw new EvaluationException("Dynamic splicing not supported.");
    }

    @Special
    @Define("comma-splice")
    public static Object commaSplice(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        throw new EvaluationException("Bad syntax. Comma-Splice not inside a backquote environment.");
    }
}
