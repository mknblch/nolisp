package de.mknblch.nolisp.core.interpreter.structs;

import de.mknblch.nolisp.core.common.FormatHelper;
import de.mknblch.nolisp.core.datatypes.Atom;
import de.mknblch.nolisp.core.datatypes.ListStruct;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Iterator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author mknblch
 */
public class ListStructTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ListStructTest.class);

    @Test
    public void testGetType() throws Exception {
        assertEquals(Atom.Type.LIST, new ListStruct().getType());
    }

    @Test
    public void testCTorRestSize() throws Exception {

        final ListStruct listStruct = new ListStruct(1, 2);
        assertFalse(listStruct.isEmpty());

        final ListStruct cdr = listStruct.cdr();
        assertFalse(cdr.isEmpty());

    }

    @Test
    public void testRestSize() throws Exception {

        assertEquals(0, new ListStruct().size());
        assertEquals(1, new ListStruct(1).size());
        assertEquals(1, new ListStruct(1, 2).cdr().size());
    }

    @Test
    public void testEmptyCTorRestSize() throws Exception {

        final ListStruct listStruct = new ListStruct();
        listStruct.add(null);
        listStruct.add(null);
        assertEquals(2, listStruct.size());
        assertFalse(listStruct.isEmpty());
    }

    @Test
    public void testCdr() throws Exception {
        ListStruct listStruct = new ListStruct("1", "2", "3", "4");
        LOGGER.debug("Testing '{}'", FormatHelper.formatAsSExpression(listStruct));


        assertEquals("1", listStruct.car());
        assertEquals("2", listStruct.cadr());
        assertEquals("3", listStruct.caddr());
        assertEquals("4", listStruct.cadddr());
    }

    @Test
    public void testLast() throws Exception {
        ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing '{}'", FormatHelper.formatAsSExpression(listStruct));
        assertEquals("3", listStruct.last().car());
    }

    @Test
    public void testAdd() throws Exception {
        final ListStruct listStruct = new ListStruct();
        LOGGER.debug("adding '1' to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("1");
        LOGGER.debug("adding '2' to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("2");
        LOGGER.debug("adding '3' to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("3");
        LOGGER.debug("result : '{}'", FormatHelper.formatAsSExpression(listStruct));
        assertEquals("1", listStruct.car());
        assertEquals("2", listStruct.cdr().car());
        assertEquals("3", listStruct.cdr().cdr().car());
        Assert.assertNull(listStruct.cdr().cdr().cdr());
    }

    @Test
    public void testAddGetNull() throws Exception {
        final ListStruct listStruct = new ListStruct();
        LOGGER.debug("adding '1' to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("1");
        LOGGER.debug("adding null to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add(null);
        LOGGER.debug("adding '3' to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("3");
        LOGGER.debug("result : '{}'", FormatHelper.formatAsSExpression(listStruct));

        assertEquals("1", listStruct.get(0));
        Assert.assertNull(listStruct.get(1));
        assertEquals("3", listStruct.get(2));
    }

    @Test
    public void testAddGetNullFirst() throws Exception {
        final ListStruct listStruct = new ListStruct();
        LOGGER.debug("adding null to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add(null);
        LOGGER.debug("adding '1' to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("1");
        LOGGER.debug("adding '3' to '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("3");
        LOGGER.debug("result : '{}'", FormatHelper.formatAsSExpression(listStruct));

        Assert.assertNull(listStruct.get(0));
        assertEquals("1", listStruct.get(1));
        assertEquals("3", listStruct.get(2));
    }

    @Test
    public void testHasSuccessor() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing successors of '{}'", FormatHelper.formatAsSExpression(listStruct));

        assertTrue(listStruct.hasSuccessor());
        assertTrue(listStruct.cdr().hasSuccessor());
        assertFalse(listStruct.cdr().cdr().hasSuccessor());
    }

    @Test
    public void testSize() throws Exception {
        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing size of '{}'", FormatHelper.formatAsSExpression(listStruct));

        assertEquals(3, listStruct.size());
        assertEquals(2, listStruct.cdr().size());
        assertEquals(1, listStruct.cdr().cdr().size());
    }

    @Test
    public void testIterator() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));
        final Iterator iterator = listStruct.iterator();

        assertTrue(iterator.hasNext());
        assertEquals("1", iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals("2", iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals("3", iterator.next());
        assertFalse(iterator.hasNext());
    }

    @Test
    public void testEmptyIterator() throws Exception {

        final ListStruct listStruct = new ListStruct();
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));
        final Iterator iterator = listStruct.iterator();

        assertFalse(iterator.hasNext());
    }

    @Test
    public void testGet() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));

        assertEquals("1", listStruct.get(0));
        assertEquals("2", listStruct.get(1));
        assertEquals("3", listStruct.get(2));
    }

    @Test(expected = Exception.class)
    public void testGetOverflow() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.get(3);
    }

    @Test
    public void testGetOrNullOverflow() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));
        Assert.assertNotNull(listStruct.nth(2));
        Assert.assertNull(listStruct.nth(3));
    }

    @Test
    public final void testListWithHeadNull() throws Exception {
        final ListStruct listStruct = new ListStruct(null, 1, 2);
        assertEquals(null, listStruct.get(0));
        assertEquals(1, listStruct.get(1));
        assertEquals(2, listStruct.get(2));
    }

    @Test
    public final void testIteratorOverNullValues() throws Exception {

        final ListStruct listStruct = new ListStruct(null, 1, 2);
        Iterator iterator = listStruct.iterator();
        assertTrue(iterator.hasNext());
        assertEquals(null, iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals(1, iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals(2, iterator.next());
    }

    @Test
    public final void testSizeForNulls() throws Exception {

        final ListStruct listStruct = new ListStruct(null, null, null);
        assertEquals(3, listStruct.size());

    }

    @Test
    public void testAttach() throws Exception {

        final ListStruct listStruct = new ListStruct("a").attach(new ListStruct("b")).attach(new ListStruct("c"));
        assertEquals("( a b c )", FormatHelper.formatPretty(listStruct));
    }
}
