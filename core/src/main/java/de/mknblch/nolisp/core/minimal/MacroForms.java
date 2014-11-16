package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.MacroForm;
import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.inspection.CloneRule;
import de.mknblch.nolisp.core.inspection.Inspector;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.Context;

import java.util.Iterator;

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

//        final CloneRule cloneRule = new CloneRule() {
//            @Override
//            public Object clone(Object element) throws Exception {
//                if (TypeHelper.isListWithSymbolHead(element, "comma")) {
//                    return interpreter.eval(((ListStruct) element).cdar(), parentContext);
//                }
//                return element;
//            }
//        };
//
//        return Inspector.cloneTree(args, cloneRule).car();

        return replace(interpreter, parentContext, args).car();
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

    public static ListStruct replace (Interpreter interpreter, Context context, ListStruct tree) throws Exception {

        System.out.printf("cloning %s%n", FormatHelper.formatAtom(tree));
        final ListStruct clone = new ListStruct();

        final Iterator<ListStruct> iterator = tree.containerIterator();

        for (; iterator.hasNext(); ) {
            ListStruct next =  iterator.next();
            final Object element = next.car();

            System.out.printf("element %s%n", FormatHelper.formatAtom(element));

            if (TypeHelper.isList(element)) {
                final ListStruct listStruct = (ListStruct) element;

                if (TypeHelper.isListWithSymbolHead(listStruct, "comma")) {
                    clone.attach(new ListStruct(interpreter.eval(listStruct.cdar(), context)));
                } else if (TypeHelper.isListWithSymbolHead(listStruct, "splice")) {

                    System.out.printf("splicing %s%n", FormatHelper.formatAtom(listStruct));
                    final ListStruct temp = listStruct.cdr();

                    for (Object o : temp) {
                        clone.add(o);
                    }

//                    clone.attach(new ListStruct(interpreter.eval(listStruct.cdar(), context)));


                } else {
                    clone.attach(new ListStruct(replace(interpreter, context, listStruct)));
                }
            } else {
                clone.attach(new ListStruct(element));
            }
        }

        System.out.printf("result %s%n", FormatHelper.formatAtom(clone));
        return clone.cdr();
    }
}
