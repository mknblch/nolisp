package de.mknblch.nolisp.core.scanner;

import de.mknblch.nolisp.core.datatypes.forms.BuiltInForm;
import de.mknblch.nolisp.core.datatypes.forms.BuiltInSpecialForm;
import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Interpreter;
import de.mknblch.nolisp.core.datatypes.ListStruct;
import de.mknblch.nolisp.core.datatypes.forms.Form;
import de.mknblch.nolisp.core.datatypes.forms.SpecialForm;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

/**
 * The AnnotationScanner constructs a set of functions from
 * annotated static methods
 *
 * @author mknblch
 */
public class AnnotationScanner {


    public static Map<String, Object> scanForFunctions(Class<?>... classes) throws FunctionDefinitionException {
        final Map<String, Object> functions = new HashMap<String, Object>();
        for (Class<?> clazz : classes) {
            final Method[] declaredMethods = clazz.getDeclaredMethods();
            for (final Method method : declaredMethods) {
                if (isForm(method)) {
                    addForm(functions, method);
                } else if (isSpecialForm(method)) {
                    addSpecialForm(functions, method);
                }
            }
        }
        return functions;
    }

    public static Map<String, Object> scanForConstants(Class<?>... classes) throws FunctionDefinitionException {
        final Map<String, Object> constants = new HashMap<String, Object>();
        for (Class<?> clazz : classes) {
            final Field[] fields = clazz.getFields();
            for (final Field field : fields) {
                if (isConstantField(field)) {
                    addConstant(constants, field);
                }
            }
        }
        return constants;
    }

    private static Form wrapNonSpecialForm(final Method method, final String symbol) {
        return new BuiltInForm() {
            @Override
            public Object eval(ListStruct args) throws Exception {
                try {
                    return method.invoke(null, args);
                } catch (InvocationTargetException e) {
                    throw unwrapInvocationException(e);
                }
            }
            @Override
            public String getSymbol() {
                return symbol;
            }
        };
    }

    private static SpecialForm wrapSpecialForm(final Method method, final String symbol) {
        return new BuiltInSpecialForm() {
            @Override
            public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
                try {
                    return method.invoke(null, interpreter, context, args);
                } catch (InvocationTargetException e) {
                    throw unwrapInvocationException(e);
                }
            }
            @Override
            public String getSymbol() {
                return symbol;
            }
        };
    }

    private static void addConstant(Map<String, Object> constants, Field field) throws FunctionDefinitionException {
        final Constant annotation = field.getAnnotation(Constant.class);
        final String[] symbols = annotation.value();
        for (String symbol : symbols) {
            try {
                if (null != constants.put(symbol, field.get(null))) {
                    throw new FunctionDefinitionException(String.format("Ambiguous constant definition for '%s'", field.getName()));
                }
            } catch (IllegalAccessException e) {
                throw new FunctionDefinitionException(e.getCause());
            }
        }
    }

    public static boolean isConstantField(Field field) throws FunctionDefinitionException {
        if (!field.isAnnotationPresent(Constant.class)) return false;
        final int modifiers = field.getModifiers();
        if (!Modifier.isStatic(modifiers))
            throw new FunctionDefinitionException("Invalid signature - FIELD must be STATIC");
        if (!Modifier.isFinal(modifiers))
            throw new FunctionDefinitionException("Invalid signature - FIELD must be FINAL");
        return true;
    }


    private static void addSpecialForm(Map<String, Object> functions, Method method) throws FunctionDefinitionException {
        final Define annotation = method.getAnnotation(Define.class);
        final String[] symbols = annotation.value();
        final SpecialForm func = wrapSpecialForm(method, symbols[0]);
        for (String symbol : symbols) {
            if (null != functions.put(symbol, func)) {
                throw new FunctionDefinitionException(String.format("Ambiguous function definition for '%s'", method.getName()));
            }
        }
    }

    private static void addForm(Map<String, Object> functions, Method method) throws FunctionDefinitionException {
        final Define annotation = method.getAnnotation(Define.class);
        final String[] symbols = annotation.value();
        final Form func = wrapNonSpecialForm(method, symbols[0]);
        for (String symbol : symbols) {
            if (null != functions.put(symbol, func)) {
                throw new FunctionDefinitionException(String.format("Ambiguous function definition for '%s'", method.getName()));
            }
        }
    }

    private static Exception unwrapInvocationException(InvocationTargetException ite) {
        final Throwable cause = ite.getCause();
        if(cause instanceof Exception) {
            return (Exception) cause;
        }
        return ite;
    }

    /**
     * checks if the method signature is suitable for Forms.
     */
    public static boolean isForm(Method method) throws FunctionDefinitionException {
        if (!method.isAnnotationPresent(Define.class)) return false;
        if (method.isAnnotationPresent(Special.class)) return false;
        if (method.getReturnType().equals(Void.TYPE))
            throw new FunctionDefinitionException("Invalid signature - METHOD must have a return value");
        if (!Modifier.isStatic(method.getModifiers()))
            throw new FunctionDefinitionException("Invalid signature - METHOD must be static");
        final Class<?>[] types = method.getParameterTypes();

        if (1 != types.length || !ListStruct.class.equals(types[0])) {
            throw new FunctionDefinitionException(String.format(
                    "Invalid method signature in '%s.%s(..)'. Expected: 'scanner(env:Context, args:ListStruct):Object'",
                    method.getDeclaringClass().getSimpleName(), method.getName()));
        }

        return true;
    }

    /**
     * checks if the method signature is suitable for SpecialForms.
     */
    public static boolean isSpecialForm(Method method) throws FunctionDefinitionException {
        if (!method.isAnnotationPresent(Define.class)) return false;
        if (!method.isAnnotationPresent(Special.class)) return false;
        if (method.getReturnType().equals(Void.TYPE))
            throw new FunctionDefinitionException("Invalid signature - METHOD must have a return value");
        if (!Modifier.isStatic(method.getModifiers()))
            throw new FunctionDefinitionException("Invalid signature - METHOD must be static");
        final Class<?>[] types = method.getParameterTypes();
        if (3 != types.length ||
                !Interpreter.class.equals(types[0]) ||
                !Context.class.equals(types[1]) ||
                !ListStruct.class.equals(types[2])) {

            throw new FunctionDefinitionException(String.format("Invalid signature in METHOD '%s.%s(..)'. Expected: 'scanner(ip:Interpreter, env:Context, args:ListStruct):Object'", method.getDeclaringClass().getSimpleName(), method.getName()));
        }

        return true;
    }
}
