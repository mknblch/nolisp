package de.mknblch.nolisp.generator;

import javax.annotation.processing.*;
import javax.lang.model.element.*;
import javax.tools.*;
import java.io.IOException;
import java.util.*;

/**
 * @author mknblch
 */
@SupportedAnnotationTypes({"*"})
public class AnnotationProcessor extends AbstractProcessor {


    private static final String DEFAULT_PACKAGE = "nolisp";

    private DialectGenerator dialectGenerator;
    private IndexGenerator indexGenerator;
    private AnnotationHelper.Filter filter;


    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        System.out.println("[PROCESSOR] AnnotationProcessor started - user.dir=" + System.getProperty("user.dir"));
        final Map<String, String> options = processingEnv.getOptions();
        System.out.println("[PROCESSOR] Options:" + options);
        buildGenerators(options);
        buildPackageFilter(options);
    }

    private void buildPackageFilter(Map<String, String> options) {
        final String packageFilterList = options.get("packageFilter");
        if (null != packageFilterList) {
            final String[] packageFilter = packageFilterList.split(",");
            filter = new AnnotationHelper.Filter() {
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
        final String outputPackage = options.get("outputPackage");
        if (null != outputPackage) {
            dialectGenerator = new DialectGenerator(outputPackage);
            indexGenerator = new IndexGenerator(outputPackage);
        } else {
            dialectGenerator = new DialectGenerator(DEFAULT_PACKAGE);
            indexGenerator = new IndexGenerator(DEFAULT_PACKAGE);
        }
    }

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

        final Set<? extends Element> functions = roundEnv.getElementsAnnotatedWith(Define.class);
        final Set<? extends Element> constants = roundEnv.getElementsAnnotatedWith(Constant.class);
        if (functions.isEmpty() && constants.isEmpty()) {
            return false;
        }
        final PackageDefinition map = AnnotationHelper.extractDefinition(functions, constants, filter);
        try {
            dialectGenerator.write(map, processingEnv.getFiler());
            indexGenerator.write(map, processingEnv.getFiler());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }

    private void printNote(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.WARNING, msg);
    }

    private void printError(String msg) {
        final Messager messager = processingEnv.getMessager();
        messager.printMessage(Diagnostic.Kind.ERROR, msg);
    }


}
