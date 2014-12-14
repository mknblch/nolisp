package de.mknblch.nolisp.dialect;

import org.atteo.classindex.ClassFilter;
import org.atteo.classindex.ClassIndex;

import java.lang.annotation.Annotation;
import java.util.ArrayList;

/**
 * @author mknblch
 */
public class ClassScanner {

    private static class PackageFilter implements ClassFilter.Predicate {

        private String packageName;

        public PackageFilter (String packageName) {
            this.packageName = packageName;
        }

        @Override
        public boolean matches(Class<?> aClass) {
            return aClass.getName().startsWith(packageName);
        }
    }

    public static Class<?>[] scanPackage(String packageName, Class<? extends Annotation> annotation) {

        final Iterable<Class<?>> classes = ClassFilter
                .any(new PackageFilter(packageName))
                .from(ClassIndex.getAnnotated(annotation));

        final ArrayList<Class<?>> list = new ArrayList<>();
        for (Class<?> clazz : classes) {
            list.add(clazz);
        }
        return list.toArray(new Class[list.size()]);
    }
}
