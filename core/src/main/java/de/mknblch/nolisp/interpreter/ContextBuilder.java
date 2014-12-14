package de.mknblch.nolisp.interpreter;

import de.mknblch.nolisp.dialect.DialectBuilder;
import de.mknblch.nolisp.dialect.FunctionDefinitionException;

/**
 * @author mknblch
 */
public class ContextBuilder {

    private static Dialect MINIMAL;

    {
        try {
            MINIMAL = DialectBuilder.buildDialect("de.mknblch.nolisp.features.minimal");
        } catch (FunctionDefinitionException e) {
            e.printStackTrace();
        }
    }

    public static Context buildContext (Dialect... dialects) {
        final Context context = new Context();
        addDialects(context, MINIMAL);
        addDialects(context, dialects);
        return context;
    }

    public static void addDialects (final Context context, Dialect... dialects) {
        for (Dialect dialect : dialects) {
            context.bindAll(dialect.features());
            context.bind(dialect.getName(), true);
        }
    }
}
