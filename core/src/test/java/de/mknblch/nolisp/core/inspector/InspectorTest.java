package de.mknblch.nolisp.core.inspector;

import de.mknblch.nolisp.core.inspection.ValueCloneRule;
import de.mknblch.nolisp.core.inspection.InspectionRule;
import de.mknblch.nolisp.core.inspection.InspectionRuleAdapter;
import de.mknblch.nolisp.core.inspection.Inspector;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.parser.Parser;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.assertEquals;

/**
 * @author mknblch
 */
public class InspectorTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(InspectorTest.class);
    private static final Parser PARSER = new Parser();

    @Test
    public void testInspect() throws Exception {
        final ListStruct program = PARSER.parse("(1 (2 nil) 3 ) 5 (6(7)) 8 9");

        final InspectionRule replaceRule = new InspectionRuleAdapter() {
            private int c = 0;
            @Override
            public void inspect(ListStruct listElement, Object car, int depth) {
                LOGGER.trace(FormatHelper.formatPretty(listElement));

                if (null == car) {
                    listElement.setCar(new ListStruct(++c));
                } else if(!TypeHelper.isList(car)) {
                    final Integer o = (Integer) listElement.car();
                    listElement.setCar(new ListStruct(c += o));
                }
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspectTree((ListStruct) program, (InspectionRule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( ( ( 1 ) ( ( 3 ) ( 4 ) ) ( 7 ) ) ( 12 ) ( ( 18 ) ( ( 25 ) ) ) ( 33 ) ( 42 ) )", program);
    }

    @Test
    public void testReplaceLists() throws Exception {

        final ListStruct program = PARSER.parse("(1 (2 (3 4) 5) 6)");

        final InspectionRule replaceRule = new InspectionRuleAdapter() {

            @Override
            public void inspect(ListStruct container, Object element, int depth) {
                LOGGER.trace(FormatHelper.formatPretty(container));
                if(TypeHelper.isList(element)) {
                    try {
                        final ListStruct listStruct = TypeHelper.asList(element);
                        final Object car = listStruct.car();
                        final Integer n = TypeHelper.asInt(car);
                        if(n % 3 == 0) container.setCar("..");
                    } catch (EvaluationException e) { }

                }
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspectTree((ListStruct) program, (InspectionRule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( ( 1 ( 2 .. 5 ) 6 ) )", program);
    }

    @Test
    public void testSum() throws Exception {

        final ListStruct program = PARSER.parse("(1 (( 2 3 (4 8) 7 8)) 9 ) ;)");

        final InspectionRule replaceRule = new InspectionRuleAdapter() {

            private int sum = 0; // ;)

            @Override
            public void inspect(ListStruct container, Object element, int depth) {
                LOGGER.trace(FormatHelper.formatPretty(container));

                if(!TypeHelper.isList(element)) {
                    final Integer n = (Integer) element;
                    container.setCar(sum += n);
                }
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspectTree((ListStruct) program, (InspectionRule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( ( 1 ( ( 3 6 ( 10 18 ) 25 33 ) ) 42 ) )", program);
    }

    @Test
    public void testDepth() throws Exception {

        final ListStruct program = PARSER.parse("0(1(2(3(4(5)))))0(1(2(3)))");

        final InspectionRule replaceRule = new InspectionRuleAdapter() {

            @Override
            public void inspect(ListStruct container, Object element, int depth) {

                if(TypeHelper.isInt(element)) container.setCar(depth);
            }
        };

        LOGGER.debug("{}", FormatHelper.formatPretty(program));
        Inspector.inspectTree((ListStruct) program, (InspectionRule) replaceRule);
        LOGGER.debug("{}", FormatHelper.formatPretty(program));

        assertASTEquals("( 0 ( 1 ( 2 ( 3 ( 4 ( 5 ) ) ) ) ) 0 ( 1 ( 2 ( 3 ) ) ) )", program);
    }

    @Test
    public void testCloneTree() throws Exception {

        final ListStruct program = PARSER.parse("( 1 2 (0 3 ) 4 ( (0 (1 (0 5)) ) 6 ) 7 )");

        final ValueCloneRule valueCloneRule = new ValueCloneRule() {

            @Override
            public Object clone(Object element) throws Exception {

                System.out.printf("cloning: %s%n", FormatHelper.formatPretty(element));
                if (TypeHelper.isList(element) && ((Integer)0).equals(((ListStruct) element).car())) {
                    return ((ListStruct) element).cdar();
                }
                return element;
            }
        };

        final ListStruct ret = Inspector.cloneTree(program, valueCloneRule);

        assertASTEquals("( ( 1 2 3 4 ( ( 1 5 ) 6 ) 7 ) )", ret);

    }

    public void assertASTEquals(String expected, ListStruct parse) {
        Assert.assertEquals(expected, FormatHelper.formatPretty(parse));
    }
}
