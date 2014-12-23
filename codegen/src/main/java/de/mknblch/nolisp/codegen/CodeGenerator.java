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

    public static final String TEMPLATE_PATH = "./codegen/src/main/resources/templates/DialectTemplate.vm";
    private VelocityEngine velocityEngine;
    private Template template;

    public CodeGenerator() {
        velocityEngine = new VelocityEngine();
        velocityEngine.init();
        template = velocityEngine.getTemplate(TEMPLATE_PATH);
    }

    public void write(PackageDefinition packageDefinition, Filer filer) throws IOException {

        for (DialectDefinition dialectDefinition : packageDefinition.getDialectDefinitions()) {

            final JavaFileObject sourceFile = filer.createSourceFile(
                    String.format("de.mknblch.nolisp.generated.%sDialect", dialectDefinition.getClassName()));
            final Writer writer = sourceFile.openWriter();
            final VelocityContext context = createVelocityContext(dialectDefinition);
            template.merge(context, writer);
            writer.close();
        }
    }

    private VelocityContext createVelocityContext(DialectDefinition dialectDefinition) {
        final VelocityContext context = new VelocityContext();

        context.put("package", "de.mknblch.nolisp.generated");
        context.put("packages", dialectDefinition.getPackages());
        context.put("dialectName", dialectDefinition.getPackageName());
        context.put("className", dialectDefinition.getClassName());
        context.put("constants", dialectDefinition.getConstants());
        context.put("functions", dialectDefinition.getFunctions());

        return context;
    }




}
