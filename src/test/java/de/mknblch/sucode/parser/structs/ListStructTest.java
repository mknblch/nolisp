package de.mknblch.sucode.parser.structs;

import de.mknblch.sucode.parser.FormatHelper;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Iterator;

import static junit.framework.Assert.assertTrue;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNull;
import static org.junit.Assert.assertEquals;

/**
 * Created by mknblch on 10.10.2014.
 */
public class ListStructTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ListStructTest.class);

    @Test
    public void testGetType() throws Exception {
        assertEquals(Atom.Type.LIST, new ListStruct().getType());
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
        LOGGER.debug("add '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("1");
        LOGGER.debug("add '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("2");
        LOGGER.debug("add '{}'", FormatHelper.formatAsSExpression(listStruct));
        listStruct.add("3");
        LOGGER.debug("add '{}'", FormatHelper.formatAsSExpression(listStruct));
        assertEquals("1", listStruct.car());
        assertEquals("2", listStruct.cdr().car());
        assertEquals("3", listStruct.cdr().cdr().car());
        assertNull(listStruct.cdr().cdr().cdr());
    }

    @Test
    public void testHasSuccessor() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing '{}'", FormatHelper.formatAsSExpression(listStruct));

        assertTrue(listStruct.hasSuccessor());
        assertTrue(listStruct.cdr().hasSuccessor());
        assertFalse(listStruct.cdr().cdr().hasSuccessor());
    }


    @Test
    public void testIterator() throws Exception {

        final ListStruct listStruct = new ListStruct("1", "2", "3");
        LOGGER.debug("Testing '{}'", FormatHelper.formatAsSExpression(listStruct));
        final Iterator iterator = listStruct.iterator();

        assertTrue(iterator.hasNext());
        assertEquals("1", iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals("2", iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals("3", iterator.next());
        assertFalse(iterator.hasNext());
    }
}
