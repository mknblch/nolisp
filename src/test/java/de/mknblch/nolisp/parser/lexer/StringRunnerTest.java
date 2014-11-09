package de.mknblch.nolisp.parser.lexer;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mknblch
 */
public class StringRunnerTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(StringRunnerTest.class);
    
    @Test
    public void testRunner() throws Exception {

        final StringRunner stringRunner = new StringRunner("ab c");
        
        stringRunner.until(new char[]{' '});

        LOGGER.debug("'{}'", stringRunner.getToken());
        

    }

}
