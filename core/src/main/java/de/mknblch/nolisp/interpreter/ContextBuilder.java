package de.mknblch.nolisp.interpreter;

/**
 * @author mknblch
 */
public class ContextBuilder {

    public static Context build (Dialect... features) {
        final Context context = new Context();
        for (Dialect dialect : features) {
            context.bindAll(dialect.features());
        }
        return context;
    }

}
