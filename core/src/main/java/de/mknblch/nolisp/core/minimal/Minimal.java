package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.interpreter.Context;
import de.mknblch.nolisp.core.interpreter.Language;
import de.mknblch.nolisp.core.scanner.AnnotationScanner;
import de.mknblch.nolisp.core.scanner.FunctionDefinitionException;

/**
 * @author mknblch
 */
public class Minimal implements Language {

    private static final Class<?>[] clazzes = {
            AccessorForms.class,
            BasicForms.class,
            ConditionForms.class,
            ComparisonForms.class,
            ConsoleForms.class,
            JavaForms.class,
            LambdaForms.class,
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
