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
public class DialectGenerator extends CodeGenerator {

    public static final String TEMPLATE_PATH = "./codegen/src/main/resources/templates/DialectTemplate.vm";

    public DialectGenerator() {
        super(TEMPLATE_PATH);
    }

    public void write(PackageDefinition packageDefinition, Filer filer) throws IOException {

        for (DialectDefinition dialectDefinition : packageDefinition.getDialectDefinitions()) {

            final VelocityContext context = createVelocityContext(dialectDefinition);
            write(context, makeFQClassName(dialectDefinition), filer);
        }
    }

    private String makeFQClassName(DialectDefinition dialectDefinition) {
        return String.format("de.mknblch.nolisp.generated.%sDialect", dialectDefinition.getClassName());
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
