package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.interpreter.Language;
import de.mknblch.nolisp.core.scanner.AnnotationScanner;
import de.mknblch.nolisp.core.scanner.FunctionDefinitionException;

import java.util.Map;

/**
 * @author mknblch
 */
public class Minimal implements Language {

    private static final Class<?>[] clazzes = {
            AccessorForms.class,
            ConditionForms.class,
            ComparisonForms.class,
            ConsoleForms.class,
            JavaForms.class,
            LambdaForms.class,
            LogicForms.class,
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

        constants.put("_lang", "nolisp.minimal");
        constants.put("_version", "0.1.0");
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
