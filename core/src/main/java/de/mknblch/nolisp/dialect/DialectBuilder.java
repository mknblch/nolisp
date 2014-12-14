package de.mknblch.nolisp.dialect;

import de.mknblch.nolisp.interpreter.Dialect;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * @author mknblch
 */
public class DialectBuilder {


    public static Dialect buildFromPackage(String packageName) throws FunctionDefinitionException {

        final Class<?>[] functions = ClassScanner.scanPackage(packageName, Define.class);
        final Class<?>[] constants = ClassScanner.scanPackage(packageName, ContainsConstant.class);

        final Dialect dialect = new EmptyDialect(packageName);
        final boolean constFound = addConstants(dialect, constants);
        final boolean funcFound = addFunctions(dialect, functions);
        if (!constFound && !funcFound) {
            throw new FunctionDefinitionException("No suitable definitions found");
        }
        return dialect;
    }

    public static Dialect buildFromClass(String dialectName, Class<?>... classes) throws FunctionDefinitionException {
        final Dialect dialect = new EmptyDialect(dialectName);
        if (!addConstants(dialect, classes) && !addFunctions(dialect, classes)) {
            throw new FunctionDefinitionException("No suitable definitions found");
        }
        return dialect;
    }

    private static boolean addConstants(Dialect dialect, Class<?>... classes) throws FunctionDefinitionException {
        // aggregate classes with @Constant annotation
        boolean found = false;
        for (Class<?> constClass : classes) {
            final Field[] fields = constClass.getFields();
            for (Field field : fields) {
                final Constant constant = field.getAnnotation(Constant.class);
                if (null == constant) continue;
                final int modifiers = field.getModifiers();
                if (    !Modifier.isPublic(modifiers) ||
                        !Modifier.isStatic(modifiers) ||
                        !Modifier.isFinal(modifiers)) {

                    throw new FunctionDefinitionException(
                            String.format("Invalid Constant definition in %s.%s", constClass.getName(), field.getName()));
                }
                try {
                    final Object value = field.get(null);
                    for (String symbol : constant.value()) {
                        dialect.features().put(symbol, value);
                    }
                } catch (IllegalAccessException e) {}

                found = true;
            }
        }
        return found;
    }

    private static boolean addFunctions(Dialect dialect, Class<?>... classes) throws FunctionDefinitionException {
        boolean found = false;
        for (Class<?> clazz : classes) {
            final Define define = clazz.getAnnotation(Define.class);
            if (null == define) continue;
            try {
                final Object instance = clazz.newInstance();
                for (String symbol : define.value()) {
                    dialect.features().put(symbol, instance);
                }
            } catch (Exception e) {
                throw new FunctionDefinitionException(String.format("Failed to instantiate class %s", clazz.getName()), e);
            }
            found = true;
        }
        return found;
    }

}
