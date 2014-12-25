package de.mknblch.nolisp.generator;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import java.util.*;

/**
 * @author mknblch
 */
public class AnnotationHelper {

    public interface Filter {
        public boolean accept (String className);
    }

    public static PackageDefinition extractDefinition(Set<? extends Element> functions, Set<? extends Element> constants, Filter filter) {

        final PackageDefinition definition = new PackageDefinition();

        extractFunctions(functions, definition, filter);
        extractConstants(constants, definition, filter);

        return definition;
    }

    private static String[] getAnnotationElementAttributes(Element element) {
        for (AnnotationMirror mirror : element.getAnnotationMirrors()) {
            for (AnnotationValue annotationValue : mirror.getElementValues().values()) {
                final List attributes = (List) annotationValue.getValue();
                final String[] ret = new String[attributes.size()];
                int n = 0;
                for (Object attribute : attributes) {
                    ret[n++] = attribute.toString();
                }
                return ret; // TODO refactor 
            }
        }
        return new String[]{};
    }


    private static void extractConstants(Set<? extends Element> elements, PackageDefinition definition, Filter filter) {
        for (Element element : elements) {
            final String fullName = fullNameOfConstantElement(element);
            if(null != filter && !filter.accept(fullName)) continue;
            final String[] annotationElementAttributes = getAnnotationElementAttributes(element);
            final String packageName = packageOfConstantElement(element);
            final DialectDefinition pack = definition.getOrCreate(packageName);
            for (String attribute : annotationElementAttributes) {
                pack.addConstant(attribute.substring(1, attribute.length() - 1), fullName);
            }
        }
    }

    private static void extractFunctions(Set<? extends Element> elements, PackageDefinition definition, Filter filter) {
        for (Element element : elements) {
            final String fullName = fullNameOfFunctionElement(element);
            if(null != filter && !filter.accept(fullName)) continue;
            final String[] annotationElementAttributes = getAnnotationElementAttributes(element);
            final String packageName = packageOfFunctionElement(element);
            final DialectDefinition pack = definition.getOrCreate(packageName);
            for (String attribute : annotationElementAttributes) {
                pack.addFunction(attribute.substring(1, attribute.length()-1), fullName);
            }
        }
    }

    private static String packageOfFunctionElement(Element element) {
        final String name = fullNameOfFunctionElement(element);
        final int pEnd = name.lastIndexOf('.');
        if (-1 != pEnd) {
            return name.substring(0, pEnd);
        }
        return "";
    }

    private static String packageOfConstantElement(Element element) {
        final String name = element.getEnclosingElement().toString();
        final int pEnd = name.lastIndexOf('.');
        if (-1 != pEnd) {
            return name.substring(0, pEnd);
        }
        return "";
    }

    private static String fullNameOfFunctionElement(Element element) {
        return String.format(element.asType().toString());
    }

    private static String fullNameOfConstantElement(Element element) {
        return String.format("%s.%s", element.getEnclosingElement().toString(), element.getSimpleName().toString());
    }

}
