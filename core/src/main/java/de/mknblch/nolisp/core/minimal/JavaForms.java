package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author mknblch
 */
public class JavaForms {

    public static final Pattern CLASS_METHOD_PATTERN = Pattern.compile("(.+)\\.(.+)");

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
    public static Object throwException(ListStruct args) throws Exception {
        throw TypeHelper.asException(args.car());
    }

    @Special
    @Define("try") // (try <form> (catch <Exception> <SYM> <form>)+)
    public static Object tryForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object tryBlock = args.car();
        final ListStruct catchBlocks = TypeHelper.asList(args.cdar());
        try {
            return interpreter.eval(tryBlock, context);
        } catch (Exception e) {
            final Class<?> exClazz = e.getClass();
            for (Object element : catchBlocks) {
                final ListStruct listStruct = TypeHelper.asList(element);
                Expectations.expectSymbolWithLiteral(listStruct.car(), "catch");
                final SymbolStruct exClassSymbol = TypeHelper.asSymbol(listStruct.cdar());
                if(Class.forName(exClassSymbol.literal).isAssignableFrom(exClazz)) {
                    final String literal = TypeHelper.symbolLiteral(listStruct.cddar());
                    final Context derive = context.derive();
                    derive.bind(literal, e);
                    return interpreter.eval(listStruct.nth(3), derive);
                }
            }
            throw e;
        }
    }


    @Special
    @Define("call") // (call <form> method-name [(<arg>+)])
    public static Object call(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object obj = interpreter.eval(args.car(), context);
        final String methodName = TypeHelper.symbolLiteral(args.cdar());
        final Object methodArgsRaw = args.cddar();
        if (null == methodArgsRaw) {
            return obj.getClass().getDeclaredMethod(methodName).invoke(obj);
        }
        final ListStruct methodArgs = TypeHelper.asList(methodArgsRaw);
        final ArrayList<Class> argTypes = new ArrayList<Class>();
        final ArrayList<Object> argValues = new ArrayList<Object>();
        for (Object arg : methodArgs) {
            final Object evaluated = interpreter.eval(arg, context);
            argTypes.add(evaluated.getClass());
            argValues.add(evaluated);
        }
        final Method method = obj.getClass().getDeclaredMethod(methodName, argTypes.toArray(new Class<?>[]{}));
        return method.invoke(obj, argValues.toArray());
    }

    @Special
    @Define({"call-static"}) // (call class.method [(<arg>+)])
    public static Object callStatic(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String fqName = TypeHelper.symbolLiteral(args.car());
        final Matcher matcher = CLASS_METHOD_PATTERN.matcher(fqName);
        if(!matcher.matches()) throw new EvaluationException("Invalid class name in static method call.");
        final String fqClassName = matcher.group(1);
        final String methodName = matcher.group(2);
        final Class<?> clazz = Class.forName(fqClassName);
        final Object methodArgsRaw = args.cdar();
        if (null == methodArgsRaw) {
            return clazz.getMethod(methodName).invoke(null);
        }
        final ListStruct methodArgs = TypeHelper.asList(methodArgsRaw);
        final ArrayList<Class> argTypes = new ArrayList<Class>();
        final ArrayList<Object> argValues = new ArrayList<Object>();
        for (Object arg : methodArgs) {
            final Object evaluated = interpreter.eval(arg, context);
            argTypes.add(evaluated.getClass());
            argValues.add(evaluated);
        }
        final Method method = clazz.getDeclaredMethod(methodName, argTypes.toArray(new Class<?>[]{}));
        return method.invoke(null, argValues.toArray());
    }

}
