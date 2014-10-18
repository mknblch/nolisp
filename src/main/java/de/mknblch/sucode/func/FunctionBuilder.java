package de.mknblch.sucode.func;

import de.mknblch.sucode.interpreter.DefaultInterpreter;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.structs.ListStruct;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

/**
 * The FunctionBuilder constructs a set of functions from
 * annotated static methods
 *
 * Created by mknblch on 11.10.2014.
 */
public class FunctionBuilder {

    public static Set<Function> build(Interpreter interpreter, Class<?>... classes) throws FunctionDefinitionException {
        final Set<Function> functions = new HashSet<Function>();
        for (Class<?> clazz : classes) {
            final Method[] declaredMethods = clazz.getDeclaredMethods();
            boolean hasSpecialForm = false;
            for (final Method method : declaredMethods) {
                // check method signature
                if (!isMethodSuitable(method)) continue;
                // get annotation data
                final Define annotation = method.getAnnotation(Define.class);
                final String[] symbols = annotation.symbol();
                hasSpecialForm |= annotation.special();
                // register with method-name as symbol
                if (symbols.length == 0) {
                    if (!functions.add(wrapMethod(method, method.getName(), annotation.special()))) {
                        throw new FunctionDefinitionException(String.format("Ambiguous function definition for '%s'", method.getName()));
                    }
                } else {
                    for (String symbol : symbols) {
                        if (!functions.add(wrapMethod(method, symbol, annotation.special()))) {
                            throw new FunctionDefinitionException(String.format("Ambiguous function definition for '%s'", symbol));
                        }
                    }
                }
            }

            if (hasSpecialForm) injectInterpreter(interpreter, clazz);

        }
        return functions;
    }

    private static void injectInterpreter(Interpreter interpreter, Class<?> clazz) {
        final Field[] fields = clazz.getDeclaredFields();
        for (Field field : fields) {
            final int modifiers = field.getModifiers();
            if (Modifier.isStatic(modifiers) && field.getType().isAssignableFrom(DefaultInterpreter.class) && field.isAnnotationPresent(InjectInterpreter.class)) {
                try {
                    field.setAccessible(true);
                    field.set(null, interpreter);
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public static Function wrapMethod(final Method method, final String symbol, final boolean specialForm) {

        return new Function() {
            @Override
            public Object eval(ListStruct args, Context context) throws EvaluationException {
                try {
                    return method.invoke(null, args, context);
                } catch (IllegalAccessException e) {
                    throw new EvaluationException(e);
                } catch (InvocationTargetException e) {
                    throw new EvaluationException(e.getCause());
                }
            }

            @Override
            public boolean isSpecialForm() {
                return specialForm;
            }

            @Override
            public String getSymbol() {
                return symbol;
            }

            @Override
            public Type getType() {
                return Type.FUNC;
            }
        };
    }

    /**
     * checks if the method signature is isMethodSuitable for Forms.
     */
    private static boolean isMethodSuitable(Method method) throws FunctionDefinitionException {
        if (!method.isAnnotationPresent(Define.class)) return false;
        if (method.getReturnType().equals(Void.TYPE))
            throw new FunctionDefinitionException("Invalid signature - method must have a return value");
        if (!Modifier.isStatic(method.getModifiers()))
            throw new FunctionDefinitionException("Invalid signature - method must be static");
        final Class<?>[] types = method.getParameterTypes();
        if (
                2 != types.length ||
                        !ListStruct.class.equals(types[0]) ||
                        !Context.class.equals(types[1])) throw new FunctionDefinitionException(String.format(
                "Invalid method signature in '%s.%s(..)'. Expected: 'func(args:ListStruct, env:Context):Object'",
                method.getDeclaringClass().getSimpleName(), method.getName()));

        return true;
    }
}
