package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.parser.Parser;
import de.mknblch.sucode.parser.Program;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static de.mknblch.sucode.helper.TypeHelper.*;
import static org.junit.Assert.*;

/**
 * @author mknblch
 */
public class InspectorTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(InspectorTest.class);
    private static final Parser PARSER = new Parser();

    @Test
    public void testInspect() throws Exception {
        final Program program = PARSER.parse("(1 (2 nil) 3 ) 5 (6(7)) 8 9");

        final Rule replaceRule = new RuleAdapter() {
            private int c = 0;
            @Override
            public void inspect(ListStruct listElement, Object car, int depth) {
                LOGGER.trace(FormatHelper.formatPretty(listElement));

                if (null == car) {
                    listElement.setCar(new ListStruct(++c));
                } else if(!isList(car)) {
                    final Integer o = (Integer) listElement.car();
                    listElement.setCar(new ListStruct(c += o));
                }
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspect((ListStruct) program, (Rule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( ( ( 1 ) ( ( 3 ) ( 4 ) ) ( 7 ) ) ( 12 ) ( ( 18 ) ( ( 25 ) ) ) ( 33 ) ( 42 ) )", program);
    }

    @Test
    public void testReplaceLists() throws Exception {

        final Program program = PARSER.parse("(1 (2 (3 4) 5) 6)");

        final Rule replaceRule = new RuleAdapter() {

            @Override
            public void inspect(ListStruct container, Object element, int depth) {
                LOGGER.trace(FormatHelper.formatPretty(container));
                if(isList(element)) {
                    try {
                        final ListStruct listStruct = asList(element);
                        final Object car = listStruct.car();
                        final Integer n = asInt(car);
                        if(n % 3 == 0) container.setCar("..");
                    } catch (EvaluationException e) { }

                }
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspect((ListStruct) program, (Rule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( ( 1 ( 2 .. 5 ) 6 ) )", program);
    }

    @Test
    public void testSum() throws Exception {

        final Program program = PARSER.parse("(1 (( 2 3 (4 8) 7 8)) 9 ) ;)");

        final Rule replaceRule = new RuleAdapter() {

            private int sum = 0; // ;)

            @Override
            public void inspect(ListStruct container, Object element, int depth) {
                LOGGER.trace(FormatHelper.formatPretty(container));

                if(!isList(element)) {
                    final Integer n = (Integer) element;
                    container.setCar(sum += n);
                }
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspect((ListStruct) program, (Rule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( ( 1 ( ( 3 6 ( 10 18 ) 25 33 ) ) 42 ) )", program);
    }

    @Test
    public void testDepth() throws Exception {

        final Program program = PARSER.parse("0(1(2(3(4(5)))))0(1(2(3)))");

        final Rule replaceRule = new RuleAdapter() {

            @Override
            public void inspect(ListStruct container, Object element, int depth) {

                if(isInt(element)) container.setCar(depth);
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspect((ListStruct) program, (Rule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( 0 ( 1 ( 2 ( 3 ( 4 ( 5 ) ) ) ) ) 0 ( 1 ( 2 ( 3 ) ) ) )", program);
    }

    public void assertASTEquals(String expected, Program parse) {
        assertEquals(expected, FormatHelper.formatPretty(parse));
    }
}
