package de.mknblch.nolisp.codegen;

import org.apache.velocity.VelocityContext;

import javax.annotation.processing.Filer;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * @author mknblch
 */
public class IndexGenerator extends CodeGenerator {

    public static final String TEMPLATE_PATH = "./codegen/src/main/resources/templates/IndexTemplate.vm";

    public IndexGenerator() {
        super(TEMPLATE_PATH);
    }

    public void write(PackageDefinition packageDefinition, Filer filer) throws IOException {

        final VelocityContext context = new VelocityContext();
        context.put("package", "de.mknblch.nolisp.generated");
        final Collection<String> packages = makeDialectNames(packageDefinition);
        context.put("packages", packages);
        write(context, "de.mknblch.nolisp.generated.Index", filer);
    }

    private Collection<String> makeDialectNames(PackageDefinition packages) {
        final Set<String> names = new HashSet<>();
        for (DialectDefinition dialectDefinition : packages.getDialectDefinitions()) {
            names.add(String.format("%sDialect", dialectDefinition.getClassName()));
        }
        return names;
    }
}
