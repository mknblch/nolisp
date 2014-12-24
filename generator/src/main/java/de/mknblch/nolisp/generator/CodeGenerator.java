package de.mknblch.nolisp.generator;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader;

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
        velocityEngine.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
        velocityEngine.setProperty("classpath.resource.loader.class", ClasspathResourceLoader.class.getName());
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
