package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.Program;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.*;

/**
 * @author mknblch
 */
public class InspectorTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(InspectorTest.class);

    private static final Parser PARSER = new Parser();

    @Test
    public void testInspect() throws Exception {

        final Program parse = PARSER.parse("(1 (2 nil) 3 ) 5 (6(7)) 8 9");

        final InspectionRule replaceRule = new InspectionRule() {

            private int c = 0;

            @Override
            public void inspect(ListStruct listElement) {

                final Object car = listElement.car();
                if (null == car) {
                    listElement.setCar(new ListStruct(++c));
                } else {
                    final Integer o = (Integer) listElement.car();
                    c += o;
                    listElement.setCar(new ListStruct(c));
                }
            }

            @Override
            public boolean followSublist(ListStruct list) {
                return true;
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(parse));
        Inspector.inspect(parse, replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(parse));

        assertASTEquals("( ( ( 1 ) ( ( 3 ) ( 4 ) ) ( 7 ) ) ( 12 ) ( ( 18 ) ( ( 25 ) ) ) ( 33 ) ( 42 ) )", parse);
    }

    @Test
    public void testReplaceLists() throws Exception {

        final Program parse = PARSER.parse("1 (3 (4 nil) 5 ) 6 7");

        final InspectionRule replaceRule = new InspectionRule() {
            @Override
            public void inspect(ListStruct list) {

            }

            @Override
            public boolean followSublist(ListStruct list) {
                if(7 != list.car()) {
                    list.setCar("oO");
                    return false;
                }

                return true;
            }

        };

        LOGGER.debug("{}", FormatHelper.formatPretty(parse));
        Inspector.inspect(parse, replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(parse));

        assertASTEquals("( 1 oO 6 7 )", parse);
    }

    public void assertASTEquals(String expected, Program parse) {
        assertEquals(expected, FormatHelper.formatPretty(parse));
    }
}
