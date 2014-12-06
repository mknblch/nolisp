package de.mknblch.nolisp.scanner;

import de.mknblch.nolisp.minimal.*;
import org.junit.Test;

/**
 * @author mknblch
 */
public class LanguageDump {


    private static final Class<?>[] clazzes = {
            ArrayForms.class,
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


    @Test
    public void testDumpMinimal() throws Exception {

        LanguageDumper.dump(clazzes);

    }
}
