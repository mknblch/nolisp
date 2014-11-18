package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

import java.lang.reflect.Constructor;
import java.util.ArrayList;

/**
 * @author mknblch
 */
public class JavaForms {

    @Special
    @Define("new") // (new java.lang.Integer [ ( args+ ) ] )
    public static Object newForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String fqn = TypeHelper.symbolLiteral(args.car());
        final Class<?> clazz = Class.forName(fqn);
        final Object initArgs = args.cdar();
        if (null != initArgs) {
            // has arguments
            return initializeNew(clazz, interpreter, context, TypeHelper.asList(initArgs));
        }
        return clazz.newInstance();
    }

    private static Object initializeNew(Class<?> clazz, Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final ArrayList<Class> classes = new ArrayList<Class>();
        final ArrayList<Object> initArgs = new ArrayList<Object>();
        for (Object arg : args) {
            final Object evaluated = interpreter.eval(arg, context);
            classes.add(evaluated.getClass());
            initArgs.add(evaluated);
        }
        final Constructor<?> declaredConstructor = clazz.getDeclaredConstructor(classes.toArray(new Class<?>[]{}));
        return declaredConstructor.newInstance(initArgs.toArray());
    }

    @Define("throw") // (throw <exception>)
    public static Object throwException(Context parentScope, ListStruct args) throws Exception {
        throw TypeHelper.asException(args.car());
    }

    @Special
    @Define("try") // (try <form> (catch <Exception> <form>)+)
    public static Object tryForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        System.out.printf("args: %s%n", FormatHelper.formatAtom(args));

        final Object tryBlock = args.car();
        final ListStruct catchBlocks = TypeHelper.asList(args.cdar());

        Object ret = null;
        try {
            ret = interpreter.eval(tryBlock, context);
        } catch (Exception e) {
            final String exName = e.getClass().getName();
            for (Object element : catchBlocks) {
                final ListStruct listStruct = TypeHelper.asList(element);
                Expectations.expectSymbolWithLiteral(listStruct.car(), "catch");
                final SymbolStruct exClassSymbol = TypeHelper.asSymbol(listStruct.cdar());
                if(exClassSymbol.literal.equals(exName)) {
                    return interpreter.eval(listStruct.cddar(), context);
                }
            }
            throw e;
        }

        return ret;
    }
}
