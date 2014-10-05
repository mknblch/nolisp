package de.mknblch.sucode.parser;

import de.mknblch.sucode.parser.structs.*;
import org.junit.Test;

/**
 * Created by pexx on 03.10.2014.
 */
public class ListStructTest {

    private static Atom atom(String id) {
        return new SymbolStruct(id);
    }

    @Test
    public void testLast() {

        ListStruct s = new ListStruct();
        s.add(atom("1"));
        s.add(atom("2"));
        s.add(atom("3"));
        System.out.println(s.last().car().getType());
        System.out.println(s.asStringDump());
    }


}
