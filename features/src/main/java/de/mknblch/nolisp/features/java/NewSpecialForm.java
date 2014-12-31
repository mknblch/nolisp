package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.annotations.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * @author mknblch
 */
@Define({"new"})
public class NewSpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String fqn = TypeHelper.getSymbolLiteral(args.car());
        final Class<?> clazz = Class.forName(fqn);
        final Object param1 = args.cadr();
        final Object param2 = args.caddr();
        if(null == param1 && null == param2) {
            // no args ctor
            return clazz.newInstance();
        } if (null == param2) {
            // 1 arg ctor; auto mode
            return makeInstanceFindTypes(clazz, interpreter, context, TypeHelper.asList(param1));
        } else {
            // 2 arg ctor; p1 = types, p2 = args
            final Object[] types = TypeHelper.convertListToArray(interpreter.evalEach(TypeHelper.asList(param1), context));
            final Object[] evaluatedArgs = TypeHelper.convertListToArray(interpreter.evalEach(TypeHelper.asList(param2), context));
            final Constructor<?> declaredConstructor = clazz.getDeclaredConstructor(Arrays.copyOf(types, types.length, Class[].class));
            return declaredConstructor.newInstance(evaluatedArgs);
        }
    }

    private static Object makeInstanceFindTypes(Class<?> clazz, Interpreter interpreter, Context context, ListStruct args) throws Exception {
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
}