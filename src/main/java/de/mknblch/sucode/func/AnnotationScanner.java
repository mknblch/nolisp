package de.mknblch.sucode.func;

import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.ast.forms.SpecialForm;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.ast.ListStruct;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The AnnotationScanner constructs a set of functions from
 * annotated static methods
 *
 * @author mknblch
 */
public class AnnotationScanner {

    public static Set<Function> scanMethods(Class<?>... classes) throws FunctionDefinitionException {
        final Set<Function> functions = new HashSet<Function>();
        for (Class<?> clazz : classes) {
            final Method[] declaredMethods = clazz.getDeclaredMethods();
            for (final Method method : declaredMethods) {
                if (isNonSpecialForm(method)) {
                    addNonSpecialForm(functions, method);
                } else if(isSpecialForm(method)) {
                    addSpecialForm(functions, method);
                }
            }
        }
        return functions;
    }

    public static Map<String, Object> scanFields(Class<?>... classes) throws FunctionDefinitionException {
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

    private static boolean isConstantField(Field field) throws FunctionDefinitionException {

        if(!field.isAnnotationPresent(Constant.class)) return false;

        final int modifiers = field.getModifiers();
        if (!Modifier.isStatic(modifiers))
            throw new FunctionDefinitionException("Invalid signature - FIELD must be STATIC");
        if (!Modifier.isFinal(modifiers))
            throw new FunctionDefinitionException("Invalid signature - FIELD must be FINAL");
        return true;
    }

    private static void addSpecialForm(Set<Function> functions, Method method) throws FunctionDefinitionException {
        final Define annotation = method.getAnnotation(Define.class);
        final String[] symbols = annotation.value();
        for (String symbol : symbols) {
            if (!functions.add(wrapSpecialForm(method, symbol))) {
                throw new FunctionDefinitionException(String.format("Ambiguous function definition for '%s'", method.getName()));
            }
        }
    }

    private static void addNonSpecialForm(Set<Function> functions, Method method) throws FunctionDefinitionException {
        final Define annotation = method.getAnnotation(Define.class);
        final String[] symbols = annotation.value();
        for (String symbol : symbols) {
            if (!functions.add(wrapNonSpecialForm(method, symbol))) {
                throw new FunctionDefinitionException(String.format("Ambiguous function definition for '%s'", method.getName()));
            }
        }
    }


    public static Function wrapNonSpecialForm(final Method method, final String symbol) {
        return new Form() {
            @Override
            public Object eval(Context context, ListStruct args) throws Exception {
                return method.invoke(null, context, args);
            }
            @Override
            public String getSymbol() {
                return symbol;
            }
        };
    }

    public static Function wrapSpecialForm(final Method method, final String symbol) {
        return new SpecialForm() {
            @Override
            public Object eval(Interpreter interpreter, Context context, ListStruct args) throws EvaluationException {
                try {
                    return method.invoke(null, interpreter, context, args);
                } catch (IllegalAccessException e) {
                    throw new EvaluationException(e.getCause());
                } catch (InvocationTargetException e) {
                    throw new EvaluationException(e.getCause());
                }
            }
            @Override
            public String getSymbol() {
                return symbol;
            }
        };
    }

    /**
     * checks if the method signature is isMethodSuitable for Forms.
     */
    private static boolean isNonSpecialForm(Method method) throws FunctionDefinitionException {
        if (!method.isAnnotationPresent(Define.class)) return false;
        if (method.isAnnotationPresent(Special.class)) return false;
        if (method.getReturnType().equals(Void.TYPE))
            throw new FunctionDefinitionException("Invalid signature - METHOD must have a return value");
        if (!Modifier.isStatic(method.getModifiers()))
            throw new FunctionDefinitionException("Invalid signature - METHOD must be static");
        final Class<?>[] types = method.getParameterTypes();
        if (
                2 != types.length ||
                        !ListStruct.class.equals(types[1]) ||
                        !Context.class.equals(types[0])) throw new FunctionDefinitionException(String.format(
                "Invalid method signature in '%s.%s(..)'. Expected: 'func(env:Context, args:ListStruct):Object'",
                method.getDeclaringClass().getSimpleName(), method.getName()));

        return true;
    }

    /**
     * checks if the method signature is isMethodSuitable for Forms.
     */
    private static boolean isSpecialForm(Method method) throws FunctionDefinitionException {
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

            throw new FunctionDefinitionException(String.format("Invalid signature in METHOD '%s.%s(..)'. Expected: 'func(ip:Interpreter, env:Context, args:ListStruct):Object'", method.getDeclaringClass().getSimpleName(), method.getName()));
        }

        return true;
    }
}
