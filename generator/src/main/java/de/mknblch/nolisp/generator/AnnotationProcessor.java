package de.mknblch.nolisp.generator;

import javax.annotation.processing.*;
import javax.lang.model.element.*;
import javax.tools.*;
import java.io.IOException;
import java.util.*;

@SupportedAnnotationTypes({"*"})
public class AnnotationProcessor extends AbstractProcessor {

    /**
     * @author mknblch
     */
    public interface Filter {
        public boolean accept (String className);
    }

    public static final String OUTPUT_PACKAGE = "outputPackage";
    private static final String DEFAULT_PACKAGE = "nolisp";
    public static final String PACKAGE_FILTER = "packageFilter";

    private DialectGenerator dialectGenerator;
    private IndexGenerator indexGenerator;
    private Filter filter;

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        printNote("AnnotationProcessor started - user.dir=" + System.getProperty("user.dir"));
        final Map<String, String> options = processingEnv.getOptions();
        printNote("Options:" + options);
        buildGenerators(options);
        buildPackageFilter(options);
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

        final Set<? extends Element> functions = roundEnv.getElementsAnnotatedWith(Define.class);
        final Set<? extends Element> constants = roundEnv.getElementsAnnotatedWith(Constant.class);
        if (functions.isEmpty() && constants.isEmpty()) return false;
        final PackageDefinition definition = new PackageDefinition();
        extractFunctions(functions, definition, (Filter) filter);
        extractConstants(constants, definition, (Filter) filter);
        try {
            dialectGenerator.write(definition, processingEnv.getFiler());
            indexGenerator.write(definition, processingEnv.getFiler());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }

    private void buildPackageFilter(Map<String, String> options) {
        final String packageFilterList = options.get(PACKAGE_FILTER);
        if (null != packageFilterList) {
            final String[] packageFilter = packageFilterList.split(",");
            filter = new Filter() {
                @Override
                public boolean accept(String className) {
                    for (String packageName : packageFilter) {
                        if(className.startsWith(packageName)) return true;
                    }
                    return false;
                }
            };
        }
    }

    private void buildGenerators(Map<String, String> options) {
        final String outputPackage = options.get(OUTPUT_PACKAGE);
        if (null != outputPackage) {
            dialectGenerator = new DialectGenerator(outputPackage);
            indexGenerator = new IndexGenerator(outputPackage);
        } else {
            dialectGenerator = new DialectGenerator(DEFAULT_PACKAGE);
            indexGenerator = new IndexGenerator(DEFAULT_PACKAGE);
        }
    }

    private void printNote(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE, msg);
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
