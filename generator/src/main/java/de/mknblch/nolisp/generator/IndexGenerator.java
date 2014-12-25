package de.mknblch.nolisp.generator;

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

    public static final String TEMPLATE_PATH = "templates/IndexTemplate.vm";

    public IndexGenerator(String outPackageName) {
        super(TEMPLATE_PATH, outPackageName);
    }

    public void write(PackageDefinition packageDefinition, Filer filer) throws IOException {

        final VelocityContext context = new VelocityContext();
        context.put("package", outPackageName);
        final Collection<String> packages = makeDialectNames(packageDefinition);
        context.put("packages", packages);
        write(context, String.format("%s.Index", outPackageName), filer);
    }

    private Collection<String> makeDialectNames(PackageDefinition packages) {
        final Set<String> names = new HashSet<>();
        for (DialectDefinition dialectDefinition : packages.getDialectDefinitions()) {
            names.add(String.format("%sDialect", dialectDefinition.getClassName()));
        }
        return names;
    }
}
