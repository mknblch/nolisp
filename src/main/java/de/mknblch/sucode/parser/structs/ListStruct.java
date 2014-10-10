package de.mknblch.sucode.parser.structs;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

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
    public ListStruct() {}

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
     * @return
     */
    public ListStruct last () {
        ListStruct temp = this;
        while (null != temp.cdr) {temp = temp.cdr;}
        return temp;
    }

    /**
     * add element to the list
     * @param atom
     */
    public void add(Object atom) {

        if (null == car) {
            car = atom;
        } else
            cons(atom);
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
     * wrap cons into ListStruct and set it as cdr in the last element of the list.
     */
    public void cons (Object cons) {
        last().cdr = new ListStruct(cons);
    }

    /**
     * basic value iterator
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
                throw new NotImplementedException();
            }
        };
    }
}
