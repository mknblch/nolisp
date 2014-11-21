package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.common.Expectations;
import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.scanner.Constant;
import de.mknblch.nolisp.core.scanner.Define;
import de.mknblch.nolisp.core.scanner.Special;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static de.mknblch.nolisp.core.common.TypeHelper.*;

/**
 * @author mknblch
 */
public class JavaForms {

    private static final Pattern CLASS_METHOD_PATTERN = Pattern.compile("(.+):(.+)");

    @Constant({"BOOLEAN", "boolean"})
    public static final Class<?> PRIMITIVE_BOOLEAN = Boolean.TYPE;

    @Constant({"BYTE", "byte"})
    public static final Class<?> PRIMITIVE_BYTE = Byte.TYPE;

    @Constant({"SHORT", "short"})
    public static final Class<?> PRIMITIVE_SHORT = Short.TYPE;

    @Constant({"CHAR", "char"})
    public static final Class<?> PRIMITIVE_CHAR = Character.TYPE;

    @Constant({"INT", "int"})
    public static final Class<?> PRIMITIVE_INT = Integer.TYPE;

    @Constant({"LONG", "long"})
    public static final Class<?> PRIMITIVE_LONG = Long.TYPE;

    @Constant({"FLOAT", "float"})
    public static final Class<?> PRIMITIVE_FLOAT = Float.TYPE;

    @Constant({"DOUBLE", "double"})
    public static final Class<?> PRIMITIVE_DOUBLE = Double.TYPE;

    @Constant({"STRING", "string"})
    public static final Class<?> PRIMITIVE_STRING = String.class;

    @Constant({"ARRAY", "array"})
    public static final Class<?> PRIMITIVE_ARRAY = Object[].class;


    @Special
    @Define("new") // (new java.lang.Integer [ ( args+ ) ] )
    public static Object newForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String fqn = symbolLiteral(args.car());
        final Class<?> clazz = Class.forName(fqn);
        final Object initArgs = args.cdar();
        if (null != initArgs) {
            // has arguments
            return initializeNew(clazz, interpreter, context, asList(initArgs));
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
        throw asException(args.car());
    }

    @Define({"classof", "classOf"}) // (throw <exception>)
    public static Object typeOf(ListStruct args) throws Exception {
        final Object car = args.car();
        if (null == car) {
            return null;
        }
        return car.getClass();
    }

    @Special
    @Define("class") // (throw <exception>)
    public static Object classForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object car = args.car();
        return Class.forName(symbolLiteral(car));
    }

    @Special
    @Define("try") // (try <form> (catch <Exception> <SYM> <form>)+)
    public static Object tryForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object tryBlock = args.car();
        final ListStruct catchBlocks = asList(args.cdar());
        try {
            return interpreter.eval(tryBlock, context);
        } catch (Exception e) {
            final Class<?> exClazz = e.getClass();
            for (Object element : catchBlocks) {
                final ListStruct listStruct = asList(element);
                Expectations.expectSymbolWithLiteral(listStruct.car(), "catch");
                final SymbolStruct exClassSymbol = asSymbol(listStruct.cdar());
                if(Class.forName(exClassSymbol.literal).isAssignableFrom(exClazz)) {
                    final String literal = symbolLiteral(listStruct.cddar());
                    final Context derive = context.derive();
                    derive.bind(literal, e);
                    return interpreter.eval(listStruct.nth(3), derive);
                }
            }
            throw e;
        }
    }

    /*
     * examples:
     * (call length "bla")
     * (call split (",") "bla,bla")
     * (call match (String) (",") "bla,bla")
     *             |p1      |p2   |p3
     */
    @Special
    @Define("call")
    public static Object call(Interpreter interpreter, Context context, ListStruct args) throws Exception {

        final String methodName = symbolLiteral(args.car());

        final Object param1 = args.cdar();
        final Object param2 = args.cddar();
        final Object param3 = args.cdddar();

        final Object[] types = convertListToArray(interpreter.evalEach(asList(findTypes(param1, param2, param3)), context));
        final Object[] params = convertListToArray(interpreter.evalEach(asList(findParams(param1, param2, param3)), context));

        final Object object = interpreter.eval(findObject(param1, param2, param3), context);
        final Class<?> clazz = object.getClass();

        return findMethod(clazz, methodName, types, params).invoke(object, params);
    }

    /*
     * examples:
     * (call-static Some:getSingleton)
     * (call-static Math:abs (42))
     * (call-static Math:abs (int) (42))
     *                       |p1   |p2
     *
     */
    @Special
    @Define({"call-static"})
    public static Object callStatic(Interpreter interpreter, Context context, ListStruct args) throws Exception {

        // split first arg into class and method name
        final String fqName = symbolLiteral(args.car());
        final Matcher matcher = CLASS_METHOD_PATTERN.matcher(fqName);
        if(!matcher.matches()) {
            throw new EvaluationException(String.format("Invalid class name in static method call, given %s.", FormatHelper.formatPretty(args.car())));
        }

        final Class<?> clazz = Class.forName(matcher.group(1));

        final Object[] param1 = convertListToArray(interpreter.evalEach(asList(args.cdar()), context));
        final Object[] param2 = convertListToArray(interpreter.evalEach(asList(args.cddar()), context));

        final Method method = findMethod(
                clazz,
                matcher.group(2),
                findStaticTypes(param1, param2),
                findStaticParams(param1, param2));

        return method.invoke(null, findStaticParams(param1, param2));
    }

    private static Method findMethod(Class<?> clazz, String methodName, Object[] types, Object[] params) throws NoSuchMethodException {

        if (null != types) {
            return clazz.getDeclaredMethod(methodName, Arrays.copyOf(types, types.length, Class[].class));
        } else if (null != params) {
            return clazz.getDeclaredMethod(methodName, determineObjectType(params));
        }
        return clazz.getDeclaredMethod(methodName);
    }

    private static Object findParams(Object param1, Object param2, Object param3) {

        if (null != param1 && null != param2 && null != param3) return param2;
        if (null != param1 && null != param2) return param1;
        return null;
    }

    private static Object findTypes(Object param1, Object param2, Object param3) {

        if(null != param1 && null != param2 && null != param3) return param1;
        return null;
    }

    private static Object findObject(Object param1, Object param2, Object param3) {

        if(null != param1 && null != param2 && null != param3) return param3;
        if(null != param1 && null != param2) return param2;
        return param1;
    }

    private static Object[] findStaticParams(Object[] param1, Object[] param2) throws EvaluationException {
        if (null == param2) return param1;
        else return param2;
    }

    private static Object[] findStaticTypes(Object[] param1, Object[] param2) throws EvaluationException {
        if (null != param2) return param1;
        return null;
    }

    private static Class<?>[] determineObjectType(Object[] objects) {
        final Class<?>[] classes = new Class<?>[objects.length];
        for (int i = 0; i < objects.length; i++) {
            final Object object = objects[i];
            classes[i] = object == null ? null : object.getClass();
        }
        return classes;
    }
}
