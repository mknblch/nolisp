package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.ContextBuilder;
import de.mknblch.nolisp.scanner.AnnotationScanner;
import de.mknblch.nolisp.scanner.FunctionDefinitionException;

/**
 * @author mknblch
 */
public class Minimal implements ContextBuilder {

    private static final Class<?>[] clazzes = {
            ArrayForms.class,
            BasicForms.class,
            ConditionForms.class,
            ContextForms.class,
            ComparisonForms.class,
            ConsoleForms.class,
            JavaForms.class,
            LambdaForms.class,
            ListForms.class,
            LogicForms.class,
            MacroForms.class,
            MathForms.class,
            PredicateForms.class,
    };

    @Override
    public Context makeContext() throws FunctionDefinitionException {
        final Context context = new Context();
        context.bindAll(AnnotationScanner.scanForConstants(clazzes));
        context.bindAll(AnnotationScanner.scanForFunctions(clazzes));
        context.bind("_lang", "nolisp.minimal");
        context.bind("_version", "0.1.0");
        return context;
    }
}
