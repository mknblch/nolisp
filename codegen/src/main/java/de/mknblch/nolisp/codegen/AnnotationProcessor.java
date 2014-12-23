package de.mknblch.nolisp.codegen;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

import javax.annotation.processing.*;
import javax.lang.model.element.*;
import javax.lang.model.util.ElementFilter;
import javax.tools.*;
import java.io.IOException;
import java.io.Writer;
import java.util.*;

/**
 * @author mknblch
 */
@SupportedAnnotationTypes({"de.mknblch.nolisp.codegen.Constant","de.mknblch.nolisp.codegen.Define"})
public class AnnotationProcessor extends AbstractProcessor {


    private CodeGenerator generator;

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        generator = new CodeGenerator();
    }



    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {


        final Set<? extends Element> functions = roundEnv.getElementsAnnotatedWith(Define.class);
        final Set<? extends Element> constants = roundEnv.getElementsAnnotatedWith(Constant.class);

        if (functions.isEmpty() && constants.isEmpty()) {
            return false;
        }

        final PackageDefinition map = AnnotationHelper.extract(functions, constants);

        try {
            generator.write(map, processingEnv.getFiler());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }

    private void printNote(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE, msg);
    }


    private void printError(String msg) {
        final Messager messager = processingEnv.getMessager();
        messager.printMessage(Diagnostic.Kind.ERROR, msg);
    }


}
