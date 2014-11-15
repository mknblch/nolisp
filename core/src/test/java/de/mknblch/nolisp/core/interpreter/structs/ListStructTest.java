package de.mknblch.nolisp.core.interpreter.structs;

import de.mknblch.nolisp.core.common.FormatHelper;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Iterator;

/**
 * @author mknblch
 */
public class ListStructTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ListStructTest.class);

    @Test
    public void testGetType() throws Exception {
        Assert.assertEquals(Atom.Type.LIST, new ListStruct().getType());
    }

    @Test
    public void testLast() throws Exception {
        ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing '{}'", FormatHelper.formatAsSExpression(listStruct));
        Assert.assertEquals("3", listStruct.last().car());
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
        Assert.assertEquals("1", listStruct.car());
        Assert.assertEquals("2", listStruct.cdr().car());
        Assert.assertEquals("3", listStruct.cdr().cdr().car());
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

        Assert.assertEquals("1", listStruct.get(0));
        Assert.assertNull(listStruct.get(1));
        Assert.assertEquals("3", listStruct.get(2));
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
        Assert.assertEquals("1", listStruct.get(1));
        Assert.assertEquals("3", listStruct.get(2));
    }

    @Test
    public void testHasSuccessor() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing successors of '{}'", FormatHelper.formatAsSExpression(listStruct));

        Assert.assertTrue(listStruct.hasSuccessor());
        Assert.assertTrue(listStruct.cdr().hasSuccessor());
        Assert.assertFalse(listStruct.cdr().cdr().hasSuccessor());
    }

    @Test
    public void testSize() throws Exception {
        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing size of '{}'", FormatHelper.formatAsSExpression(listStruct));

        Assert.assertEquals(3, listStruct.size());
        Assert.assertEquals(2, listStruct.cdr().size());
        Assert.assertEquals(1, listStruct.cdr().cdr().size());
    }

    @Test
    public void testIterator() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));
        final Iterator iterator = listStruct.iterator();

        Assert.assertTrue(iterator.hasNext());
        Assert.assertEquals("1", iterator.next());
        Assert.assertTrue(iterator.hasNext());
        Assert.assertEquals("2", iterator.next());
        Assert.assertTrue(iterator.hasNext());
        Assert.assertEquals("3", iterator.next());
        Assert.assertFalse(iterator.hasNext());
    }
    @Test
    public void testEmptyIterator() throws Exception {

        final ListStruct listStruct = new ListStruct();
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));
        final Iterator iterator = listStruct.iterator();

        Assert.assertFalse(iterator.hasNext());
    }

    @Test
    public void testGet() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing iterator for '{}'", FormatHelper.formatAsSExpression(listStruct));

        Assert.assertEquals("1", listStruct.get(0));
        Assert.assertEquals("2", listStruct.get(1));
        Assert.assertEquals("3", listStruct.get(2));
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
        Assert.assertNotNull(listStruct.getOrNull(2));
        Assert.assertNull(listStruct.getOrNull(3));
    }

    @Test
    public final void testListWithHeadNull() throws Exception {
        final ListStruct listStruct = new ListStruct(null, 1, 2);
        Assert.assertEquals(null, listStruct.get(0));
        Assert.assertEquals(1, listStruct.get(1));
        Assert.assertEquals(2, listStruct.get(2));
    }

    @Test
    public final void testIteratorOverNullValues() throws Exception {

        final ListStruct listStruct = new ListStruct(null, 1, 2);
        Iterator iterator = listStruct.iterator();
        Assert.assertTrue(iterator.hasNext());
        Assert.assertEquals(null, iterator.next());
        Assert.assertTrue(iterator.hasNext());
        Assert.assertEquals(1, iterator.next());
        Assert.assertTrue(iterator.hasNext());
        Assert.assertEquals(2, iterator.next());
    }

    @Test
    public final void testSizeForNulls() throws Exception {

        final ListStruct listStruct = new ListStruct(null, null, null);
        Assert.assertEquals(3, listStruct.size());

    }
}
