package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author mknblch
 */
public class JavaForms {

    private static final Pattern CLASS_METHOD_PATTERN = Pattern.compile("(.+):(.+)");

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
        final Constructor<?> declaredConstructor = clazz.getDeclaredConstructor(classes.toArray(new Class<?>[classes.size()]));
        return declaredConstructor.newInstance(initArgs.toArray());
    }

    @Define("throw") // (throw <exception>)
    public static Object throwException(ListStruct args) throws Exception {
        throw TypeHelper.asException(args.car());
    }

    @Define("classof") // (throw <exception>)
    public static Object typeOf(ListStruct args) throws Exception {
        final Object car = args.car();
        if (null == car) {
            return null;
        }
        return car.getClass();
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
    @Define("call") // (call <obj> method { (types) (args) }? )
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
    @Define({"call-static"}) // (call class.method (..)? (<arg>+)? )
    public static Object callStatic(Interpreter interpreter, Context context, ListStruct args) throws Exception {

        // split first arg into class and method anme
        final String fqName = TypeHelper.symbolLiteral(args.car());
        final Matcher matcher = CLASS_METHOD_PATTERN.matcher(fqName);
        if(!matcher.matches()) throw new EvaluationException("Invalid class name in static method call.");
        final String fqClassName = matcher.group(1);
        final String methodName = matcher.group(2);

        // retrieve the class
        final Class<?> clazz = Class.forName(fqClassName);

        // retrieve method type definition
        final Object methodTypesRaw = interpreter.eval(args.cdar(), context);
        // retrieve the actual arguments
        final Object methodArgsRaw = interpreter.eval(args.cddar(), context);




        // no types & args given, invoke noArgs
        if (null == methodTypesRaw && null == methodArgsRaw) {
            return clazz.getMethod(methodName).invoke(null);
        }
        // save cast to raw object array
        final Object[] methodTypes = TypeHelper.asArray(methodTypesRaw);
        // save cast to list
        final ListStruct methodArgsList = TypeHelper.asList(methodArgsRaw);

        final Object[] argValues = new Object[methodTypes.length];

        int i = 0;
        for (Object arg : methodArgsList) {
            final Object evaluated = interpreter.eval(arg, context);
            argValues[i++] = evaluated;
        }

        final Method method = clazz.getDeclaredMethod(methodName, Arrays.copyOf(methodTypes, methodTypes.length, Class[].class));
        return method.invoke(null, argValues);
    }

    private static Method findMethod(Class<?> clazz, String methodName, ListStruct param1, ListStruct param2)
            throws EvaluationException, NoSuchMethodException {

        // (call-static String:valueOf (42))
        if (null == param1 && null == param2) {
            return clazz.getMethod(methodName);
        }
        if (null != param1 && null == param2) {
            // param1 given and param2 not.. p1 is value list, determine types by inspecting values
            final Class<?>[] types = determineObjectType(TypeHelper.convertListToArray(param1));
            return clazz.getMethod(methodName, types);
        } else {
            // param1 and param2 given, p1 is type list, p2 value list
            final Object[] typeList = TypeHelper.convertListToArray(param1);
            return clazz.getMethod(methodName, Arrays.copyOf(typeList, typeList.length, Class[].class));
        }
    }

    private static Class<?>[] determineObjectType(Object[] objects) {
        final Class<?>[] classes = new Class<?>[objects.length];
        for (int i = 0; i < objects.length; i++) {
            final Object object = objects[i];
            classes[i] = object == null ? null : object.getClass();
        }
        return classes;
    }

    private static Object invoke (Method method, Object obj, Class<?> argTypes, Object[] args)  {

    }
}
