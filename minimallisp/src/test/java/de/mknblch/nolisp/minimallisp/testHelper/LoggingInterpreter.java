package de.mknblch.nolisp.minimallisp.testHelper;

import de.mknblch.nolisp.core.helper.FormatHelper;
import de.mknblch.nolisp.core.interpreter.CoreInterpreter;
import de.mknblch.nolisp.core.interpreter.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mknblch
 */
public class LoggingInterpreter extends CoreInterpreter {

    private static final Logger LOGGER = LoggerFactory.getLogger(LoggingInterpreter.class);

    @Override
    public Object eval(Object obj, Context context) throws Exception {
        final Object evaluated = super.eval(obj, context);
        LOGGER.trace("{} => {}", FormatHelper.formatPretty(obj), FormatHelper.formatPretty(evaluated));
        return evaluated;
    }


}
