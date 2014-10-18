package de.mknblch.sucode.interpreter;

import de.mknblch.sucode.helper.FormatHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Created by mknblch on 18.10.2014.
 */
public class LoggingInterpreter extends DefaultInterpreter {

    private static final Logger LOGGER = LoggerFactory.getLogger(LoggingInterpreter.class);

    @Override
    public Object eval(Object obj, Context context) throws Exception {
        final Object evaluated = super.eval(obj, context);
        LOGGER.trace("{} => {}", FormatHelper.formatPretty(obj), FormatHelper.formatPretty(evaluated));
        return evaluated;
    }


}
