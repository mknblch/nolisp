package de.mknblch.nolisp.core.scanner;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/**
 * @author mknblch
 */
public class LanguageDumper {

    private static class FuncMap extends HashMap<String, ArrayList<String>> {
        public void add (String sourceClass, String funcName) {
            ArrayList<String> strings = get(sourceClass);
            if (null == strings) {
                strings = new ArrayList<String>();
                put(sourceClass, strings);
            }
            strings.add(funcName);
        }
        public int size() {
            int r = 0;
            for (ArrayList<String> fNames : this.values()) {
                r += fNames.size();
            }
            return r;
        }
    }

    public static void dump (Class<?>... classes) throws FunctionDefinitionException {
        System.out.println(exportMarkdown(classes));
    }

    public static String exportMarkdown(Class<?>... classes) throws FunctionDefinitionException {
        // scan
        final FuncMap specialForms = new FuncMap();
        final FuncMap stdForms = new FuncMap();
        for (Class<?> clazz : classes) {
            final String simpleName = clazz.getSimpleName().replaceAll("Forms$", "");
            final Method[] declaredMethods = clazz.getDeclaredMethods();
            for (final Method method : declaredMethods) {
                final Define annotation = method.getAnnotation(Define.class);
                if (AnnotationScanner.isForm(method)) {
                    final String[] value = annotation.value();
                    final String fname = value.length > 1 ? Arrays.toString(value) : value[0];
                    stdForms.add(simpleName, fname);
                } else if (AnnotationScanner.isSpecialForm(method)) {
                    final String[] value = annotation.value();
                    final String fname = value.length > 1 ? Arrays.toString(value) : value[0];
                    specialForms.add(simpleName, fname);
                }
            }
        }

        // export
        final StringBuilder buffer = new StringBuilder();
        buffer.append(String.format("## %02d Special Forms%n", specialForms.size()));
        for (String sourceClass : specialForms.keySet()) {
            buffer.append(String.format("%n### %s:%n", sourceClass));

            final ArrayList<String> functions = specialForms.get(sourceClass);
            for (String fName : functions) {
                buffer.append(String.format("\t%s%n", fName));
            }
        }
        buffer.append(String.format("%n## %02d Forms%n", stdForms.size()));
        for (String sourceClass : stdForms.keySet()) {
            buffer.append(String.format("%n### %s:%n", sourceClass));

            final ArrayList<String> functions = stdForms.get(sourceClass);
            for (String fName : functions) {
                buffer.append(String.format("\t%s%n", fName));
            }
        }

        return buffer.toString();
    }

}
