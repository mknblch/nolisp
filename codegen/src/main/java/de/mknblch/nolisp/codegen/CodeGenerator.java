package de.mknblch.nolisp.codegen;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

import javax.annotation.processing.Filer;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.Writer;

/**
 * @author mknblch
 */
public class CodeGenerator {

    private final VelocityEngine velocityEngine;
    private final Template template;

    public CodeGenerator(String templatePath) {
        velocityEngine = new VelocityEngine();
        velocityEngine.init();
        template = velocityEngine.getTemplate(templatePath);
    }

    public void write(VelocityContext context, CharSequence fullQualifiedClassName, Filer filer) throws IOException {
        final JavaFileObject sourceFile = filer.createSourceFile(fullQualifiedClassName);
        final Writer writer = sourceFile.openWriter();
        template.merge(context, writer);
        writer.close();
    }
}
