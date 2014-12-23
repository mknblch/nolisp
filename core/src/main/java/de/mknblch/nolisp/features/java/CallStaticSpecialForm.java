package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.dialect.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.codegen.Define;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static de.mknblch.nolisp.common.TypeHelper.*;

/**
 * @author mknblch
 */
@Define({"call-static"})
public class CallStaticSpecialForm extends BuiltInSpecialForm {

    private static final Pattern CLASS_PATTERN = Pattern.compile("(.+):(.+)");

    private static Method findMethod(Class<?> clazz, String methodName, Object[] types, Object[] params) throws NoSuchMethodException {

        if (null != types) {
            return clazz.getDeclaredMethod(methodName, Arrays.copyOf(types, types.length, Class[].class));
        } else if (null != params) {
            return clazz.getDeclaredMethod(methodName, determineObjectType(params));
        }
        return clazz.getDeclaredMethod(methodName);
    }

    private static Object[] findStaticParams(Object[] param1, Object[] param2) throws EvaluationException {
        if (null == param2) return param1;
        else return param2;
    }

    private static Object[] findStaticTypes(Object[] param1, Object[] param2) throws EvaluationException {
        if (null != param2) return param1;
        return null;
    }

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        // split first arg into class and method packageName
        final String fqName = getSymbolLiteral(args.car());
        final Matcher matcher = CLASS_PATTERN.matcher(fqName);
        if (!matcher.matches()) {
            throw new EvaluationException(String.format("Invalid literal in static method call, given %s.", FormatHelper.formatPretty(args.car())));
        }
        final Class<?> clazz = Class.forName(matcher.group(1));
        final Object[] param1 = convertListToArray(interpreter.evalEach(asList(args.cadr()), context));
        final Object[] param2 = convertListToArray(interpreter.evalEach(asList(args.caddr()), context));

        final Method method = findMethod(
                clazz,
                matcher.group(2),
                findStaticTypes(param1, param2),
                findStaticParams(param1, param2));

        return method.invoke(null, findStaticParams(param1, param2));
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