package de.mknblch.nolisp.dialect;

import org.atteo.classindex.ClassFilter;
import org.atteo.classindex.ClassIndex;

import java.lang.annotation.Annotation;
import java.util.ArrayList;

/**
 * @author mknblch
 */
public class ClassScanner {

    public static Class<?>[] scanPackage(String packageName, Class<? extends Annotation> annotation) {

        final Iterable<Class<?>> classes = ClassIndex.getAnnotated(annotation);

        final ArrayList<Class<?>> list = new ArrayList<>();
        for (Class<?> clazz : classes) {
            if(!clazz.getName().startsWith(packageName)) continue;
            list.add(clazz);
        }
        return list.toArray(new Class[list.size()]);
    }
}
