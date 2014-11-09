package de.mknblch.nolisp.minimallisp;

import de.mknblch.nolisp.annotations.AnnotationScanner;
import de.mknblch.nolisp.annotations.FunctionDefinitionException;
import de.mknblch.nolisp.interpreter.Language;

import java.util.Map;

/**
 * @author mknblch
 */
public class MinimalLisp implements Language {

    private static final Class<?> [] clazzes = {
            ConditionForms.class,
            ConsoleForms.class,
            LambdaForms.class,
            MacroForms.class,
            MathForms.class,
            PredicateForms.class,
            BasicForms.class
    };

    private final Map<String, Object> constants;
    private final Map<String, Object> functions;

    public MinimalLisp() throws FunctionDefinitionException {
        constants = AnnotationScanner.scanForConstants(clazzes);
        functions = AnnotationScanner.scanForFunctions(clazzes);
    }

    @Override
    public String getName() {
        return "Minimal Lisp";
    }

    @Override
    public String getVersion() {
        return "0.1.0";
    }

    @Override
    public Map<String, Object> getConstants() {
        return constants;
    }

    @Override
    public Map<String, Object> getFunctions() {
        return functions;
    }
}
