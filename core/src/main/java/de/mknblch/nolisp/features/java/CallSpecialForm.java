package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.codegen.Define;

import java.lang.reflect.Method;
import java.util.Arrays;

import static de.mknblch.nolisp.common.TypeHelper.*;

/**
 * @author mknblch
 */
@Define({"call"})
public class CallSpecialForm extends BuiltInSpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String methodName = getSymbolLiteral(args.car());
        final Object param1 = args.cadr();
        final Object param2 = args.caddr();
        final Object param3 = args.cadddr();
        final Object[] types = convertListToArray(interpreter.evalEach(asList(findTypes(param1, param2, param3)), context));
        final Object[] params = convertListToArray(interpreter.evalEach(asList(findParams(param1, param2, param3)), context));

        final Object object = interpreter.eval(findObject(param1, param2, param3), context);
        final Class<?> clazz = object.getClass();

        return findMethod(clazz, methodName, types, params).invoke(object, params);
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

        if (null != param1 && null != param2 && null != param3) return param1;
        return null;
    }

    private static Object findObject(Object param1, Object param2, Object param3) {

        if (null != param1 && null != param2 && null != param3) return param3;
        if (null != param1 && null != param2) return param2;
        return param1;
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