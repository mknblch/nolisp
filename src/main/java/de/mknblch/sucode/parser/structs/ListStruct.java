package de.mknblch.sucode.parser.structs;

import java.util.Iterator;

/**
 * Created by mknblch on 03.10.2014.
 */
public class ListStruct implements Atom, Iterable {

    private Object car = null;
    private ListStruct cdr = null;

    /**
     * constructs an empty list
     */
    public ListStruct() {
    }

    public ListStruct(Object... items) {
        for (int i = 0; i < items.length; i++) {
            add(items[i]);
        }
    }

    @Override
    public Type getType() {
        return Type.LIST;
    }

    /**
     * retrieve last listElement of the list
     *
     * @return
     */
    public ListStruct last() {
        ListStruct temp = this;
        while (null != temp.cdr) {
            temp = temp.cdr;
        }
        return temp;
    }

    /**
     * add element to the list
     *
     * @param atom
     */
    public void add(Object atom) {

        if (null == car) {
            car = atom;
        } else
            last().cdr = new ListStruct(atom);
    }

    /**
     * determine if this element has a successor
     */
    public boolean hasSuccessor() {
        return null != cdr;
    }

    /**
     * get current list element.
     */
    public Object car() {
        return car;
    }

    /**
     * get rest list.
     */
    public ListStruct cdr() {
        return cdr;
    }

    /**
     * return number of elements in the list
     */
    public int size() {
        if (null == car) {
            return 0;
        }
        int i = 1;
        for (ListStruct temp = this; null != temp.cdr; i++) {
            temp = temp.cdr;
        }
        return i;
    }

    /**
     * value iterator
     */
    @Override
    public Iterator iterator() {
        return new Iterator() {
            private ListStruct head = ListStruct.this;

            @Override
            public boolean hasNext() {
                return null != head;
            }

            @Override
            public Object next() {
                final Object car = head.car();
                head = head.cdr();
                return car;
            }

            @Override
            public void remove() {
                throw new RuntimeException("removeLocal() not implemented");
            }
        };
    }
}
