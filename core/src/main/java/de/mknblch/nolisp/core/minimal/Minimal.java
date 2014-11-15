package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.scanner.AnnotationScanner;
import de.mknblch.nolisp.core.scanner.FunctionDefinitionException;
import de.mknblch.nolisp.core.interpreter.Language;

import java.util.Map;

/**
 * @author mknblch
 */
public class Minimal implements Language {

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

    public Minimal() throws FunctionDefinitionException {
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

    @Override
    public void include(Language language) {
        constants.putAll(language.getConstants());
        functions.putAll(language.getFunctions());
    }
}
