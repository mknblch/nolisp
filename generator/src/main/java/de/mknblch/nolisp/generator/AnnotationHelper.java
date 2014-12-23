package de.mknblch.nolisp.generator;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import java.util.*;

/**
 * @author mknblch
 */
public class AnnotationHelper {

    public static PackageDefinition extract(Set<? extends Element> functions, Set<? extends Element> constants) {

        final PackageDefinition definition = new PackageDefinition();

        extractFunctions(functions, definition);
        extractConstants(constants, definition);

        return definition;
    }

    public static String[] getAnnotationElementAttributes(Element element) {
        for (AnnotationMirror mirror : element.getAnnotationMirrors()) {
            for (AnnotationValue annotationValue : mirror.getElementValues().values()) {
                final List attributes = (List) annotationValue.getValue();
                final String[] ret = new String[attributes.size()];
                int n = 0;
                for (Object attribute : attributes) {
                    ret[n++] = attribute.toString();
                }
                return ret;
            }
        }
        return new String[]{};
    }


    public static void extractConstants(Set<? extends Element> elements, PackageDefinition definition) {
        for (Element element : elements) {
            final String[] annotationElementAttributes = getAnnotationElementAttributes(element);
            final String packageName = packageOfConstantElement(element);
            final String fullName = fullNameOfConstantElement(element);
            final DialectDefinition pack = definition.getOrCreate(packageName);
            for (String attribute : annotationElementAttributes) {
                pack.addConstant(attribute.substring(1, attribute.length() - 1), fullName);
            }
        }
    }

    public static void extractFunctions(Set<? extends Element> elements, PackageDefinition definition) {
        for (Element element : elements) {
            final String[] annotationElementAttributes = getAnnotationElementAttributes(element);
            final String packageName = packageOfFunctionElement(element);
            final String fullName = fullNameOfFunctionElement(element);
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
