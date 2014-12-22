package de.mknblch.nolisp.codegen;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import javax.tools.FileObject;
import javax.tools.JavaFileManager;
import javax.tools.StandardLocation;
import javax.xml.stream.Location;
import java.io.IOException;
import java.io.Writer;
import java.util.Set;

/**
 * @author mknblch
 */
@SupportedAnnotationTypes({"de.mknblch.nolisp.codegen.Constant","de.mknblch.nolisp.codegen.Define"})
public class AnnotationProcessor extends AbstractProcessor {

    public static final String TEMPLATE_PATH = "./codegen/src/main/resources/templates/DialectTemplate.vm";
    private VelocityEngine velocityEngine;
    private Template template;

    @Override
    public synchronized void init(ProcessingEnvironment processingEnv) {
        super.init(processingEnv);
        velocityEngine = new VelocityEngine();
        velocityEngine.init();
        template = velocityEngine.getTemplate(TEMPLATE_PATH);
    }



    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

        printNote("processing.." + annotations);

        final Set<? extends Element> functions = roundEnv.getElementsAnnotatedWith(Define.class);
        final Set<? extends Element> constants = roundEnv.getElementsAnnotatedWith(Constant.class);

        if (functions.isEmpty() && constants.isEmpty()) {
            return false;
        }

        processFunctions(functions);
        processConstants(constants);
        
        return true;
    }

    private void processFunctions(Set<? extends Element> functions) {

        printNote("Functions..");

        final VelocityContext context = new VelocityContext();

        context.put("fields", functions);

        try {
            final FileObject resource = processingEnv.getFiler().createResource(StandardLocation.SOURCE_OUTPUT, "abc", "functions");
            final Writer writer = resource.openWriter();
            template.merge(context, writer);
            writer.close();
        } catch (IOException e) {

            printError(e.getMessage());


        }

    }

    private void processConstants(Set<? extends Element> constants) {

        printNote("Constants..");

        for (Element constant : constants) {
            printNote(constant.toString());
        }
    }

    private void printNote(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE, msg);
    }


    private void printError(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, msg);
    }


}
