package de.mknblch.nolisp.scanner;

import de.mknblch.nolisp.interpreter.Dialect;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

/**
 * @author mknblch
 */
public class DialectBuilder {

    public static Dialect build (String packageName) throws FunctionDefinitionException {
        final Package pack = Package.getPackage(packageName);
        if (null == pack) {
            throw new FunctionDefinitionException(String.format("Package %s not found", packageName));
        }
        final Annotation[] annotations = pack.getAnnotations();
        if (0 == annotations.length) {
            throw new FunctionDefinitionException(String.format("The package %s contains no suitable definitions", packageName));
        }
        final Dialect dialect = new EmptyDialect();
        if (!buildConstants(annotations, dialect) && !buildFunctions(annotations, dialect)) {
            throw new FunctionDefinitionException(String.format("The package %s contains no suitable definitions", packageName));
        }
        return dialect;
    }

    private static boolean buildConstants(Annotation[] annotations, Dialect dialect) throws FunctionDefinitionException {
        // aggregate classes with @Constant annotation
        boolean found = false;
        final Set<Class<?>> constClasses = new HashSet<>();
        for (Annotation annotation : annotations) {
            if (!(annotation instanceof Constant)) continue;
            constClasses.add(annotation.getClass());
        }
        for (Class<?> constClass : constClasses) {
            final Field[] fields = constClass.getFields();
            for (Field field : fields) {
                final Constant constant = field.getAnnotation(Constant.class);
                if (null == constant) continue;
                final int modifiers = field.getModifiers();
                if (!Modifier.isStatic(modifiers) || !Modifier.isFinal(modifiers)) {
                    throw new FunctionDefinitionException(String.format("Invalid Constant definition in %s.%s", constClass.getName(), field.getName()));
                }
                for (String symbol : constant.value()) {
                    dialect.features().put(symbol, field);
                }
                found = true;
            }
        }
        return found;
    }

    private static boolean buildFunctions(Annotation[] annotations, Dialect dialect) throws FunctionDefinitionException {
        boolean found = false;
        for (Annotation annotation : annotations) {
            if (!(annotation instanceof Define)) continue;
            final Define define = (Define) annotation;
            final Class<? extends Annotation> clazz = annotation.getClass();
            Annotation instance;
            try {
                 instance = clazz.newInstance();
            } catch (Exception e) {
                throw new FunctionDefinitionException(String.format("Failed to instantiate class %s", clazz.getName()), e);
            }
            for (String symbol : define.value()) {
                dialect.features().put(symbol, instance);
            }
            found = true;
        }
        return found;
    }

}
