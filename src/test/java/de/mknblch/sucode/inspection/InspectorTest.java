package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.helper.TypeHelper;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.Program;
import org.junit.Assert;
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

        final Program parse = PARSER.parse("(1 (2 nil) 4 ) 5");

        final InspectionRule replaceRule = new InspectionRule() {
            @Override
            public void inspect(ListStruct listElement) {

                final Object car = listElement.car();
                if (null == car) {
                    listElement.setCar(new ListStruct(null));
                } else {
                    listElement.setCar(42);
                }
            }

            @Override
            public boolean visitLists() {
                return false;
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(parse));
        Inspector.inspect(parse, replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(parse));

        assertASTEquals("( ( 42 ( 42 ( nil ) ) 42 ) 42 )", parse);
    }

    @Test
    public void testInspectVisit() throws Exception {

        final Program parse = PARSER.parse("1 (1 (2 nil) 4 ) 5 6 (7)");

        final InspectionRule replaceRule = new InspectionRule() {
            @Override
            public void inspect(ListStruct listElement) {

                final Object car = listElement.car();
                if (null == car) {
                    listElement.setCar(new ListStruct(null));
                } else if (TypeHelper.isList(car)) {
                    listElement.setCar("oO");
                } else {
                    listElement.setCar(42);
                }
            }

            @Override
            public boolean visitLists() {
                return true;
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(parse));
        Inspector.inspect(parse, replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(parse));

        assertASTEquals("( 42 oO 42 42 oO )", parse);
    }

    public void assertASTEquals(String expected, Program parse) {
        assertEquals(expected, FormatHelper.formatPretty(parse));
    }
}
