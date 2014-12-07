package de.mknblch.nolisp.interpreter;

/**
 * @author mknblch
 */
public class ContextBuilder {

    public static Context build (Feature... features) {
        final Context context = new Context();
        for (Feature feature : features) {
            context.bindAll(feature);
        }
        return context;
    }

}
