package de.mknblch.nolisp.generator;

import org.apache.velocity.VelocityContext;

import javax.annotation.processing.Filer;
import java.io.IOException;

/**
 * @author mknblch
 */
public class DialectGenerator extends CodeGenerator {

    public static final String TEMPLATE_PATH = "templates/DialectTemplate.vm";

    public DialectGenerator(String outPackageName) {
        super(TEMPLATE_PATH, outPackageName);
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
