package de.mknblch.nolisp.generator;

import javax.annotation.processing.*;
import javax.lang.model.element.*;
import javax.tools.*;
import java.io.IOException;
import java.util.*;

/**
 * @author mknblch
 */
@SupportedAnnotationTypes({"de.mknblch.nolisp.generator.Constant","de.mknblch.nolisp.generator.Define"})
public class AnnotationProcessor extends AbstractProcessor {


    private DialectGenerator dialectGenerator;
    private IndexGenerator indexGenerator;


    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);

        System.out.println("[PROCESSOR] AnnotationProcessor started - user.dir=" + System.getProperty("user.dir"));

        dialectGenerator = new DialectGenerator();
        indexGenerator = new IndexGenerator();
    }



    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {


        final Set<? extends Element> functions = roundEnv.getElementsAnnotatedWith(Define.class);
        final Set<? extends Element> constants = roundEnv.getElementsAnnotatedWith(Constant.class);

        if (functions.isEmpty() && constants.isEmpty()) {
            return false;
        }

        final PackageDefinition map = AnnotationHelper.extractDefinition(functions, constants);

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
