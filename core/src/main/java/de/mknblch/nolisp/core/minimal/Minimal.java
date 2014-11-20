package de.mknblch.nolisp.core.minimal;

import de.mknblch.nolisp.core.interpreter.Language;
import de.mknblch.nolisp.core.scanner.AnnotationScanner;
import de.mknblch.nolisp.core.scanner.FunctionDefinitionException;

import java.util.HashMap;
import java.util.Map;

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

    private final Map<String, Object> contextMap = new HashMap<>();

    public Minimal() throws FunctionDefinitionException {
        contextMap.putAll(AnnotationScanner.scanForConstants(clazzes));
        contextMap.putAll(AnnotationScanner.scanForFunctions(clazzes));
        contextMap.put("_lang", "nolisp.minimal");
        contextMap.put("_version", "0.1.0");
    }

    @Override
    public Map<String, Object> getContextMap() {
        return contextMap;
    }
}
