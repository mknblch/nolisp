package de.mknblch.sucode.ast;

import java.util.Iterator;

/**
 * Created by mknblch on 03.10.2014.
 */
public class ListStruct implements Atom, Iterable {

    private boolean isEmptyList = false;
    private Object car = null;
    private ListStruct cdr = null;

    /**
     * constructs an empty list
     */
    public ListStruct(Object car, Object... rest) {
        this.car = car;
        for (int i = 0; i < rest.length; i++) {
            append(rest[i]);
        }
    }

    public ListStruct() {
        isEmptyList = true;
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
     * add element to the list if:
     * the list has a successor  the element is appended to the end.
     * if not
     *
     * @param atom
     */
    public ListStruct append(Object atom) {

        if (cdr == null) cdr = new ListStruct(atom);
        else last().cdr = new ListStruct(atom);

        return this;
    }

    public void add(Object o) {
        if (isEmptyList) {
            isEmptyList = false;
            car = o;
        } else {
            append(o);
        }
    }

    public Object get(int n) {
        ListStruct temp = this;
        for (int i = n; i > 0; i--) {
            if (null == temp.cdr)
                throw new IndexOutOfBoundsException(String.format("Index: %d, Size: %d", n, this.size()));
            temp = temp.cdr;
        }
        return temp.car();
    }

    /**
     * determine if this element has a successor
     */
    public boolean hasSuccessor() {
        return null != cdr;
    }

    public Object getOrNull(int n) {
        ListStruct temp = this;
        for (int i = n; i > 0; i--) {
            if (null == temp.cdr) return null;
            temp = temp.cdr;
        }
        return temp.car();
    }

    /**
     * get current list element.
     */
    public Object car() {
        return car;
    }

    public Object caar() {
        if (null != car && car instanceof ListStruct) return ((ListStruct) car).car();
        return null;
    }

    public Object caaar() {
        if (null != car && car instanceof ListStruct) {
            ListStruct caar = ((ListStruct) caar());
            if (null != caar && caar instanceof ListStruct) {
                return ((ListStruct) caar).car;
            }
        }
        return null;
    }

    /**
     * get rest list.
     */
    public ListStruct cdr() {
        return cdr;
    }

    public Object cdar() {
        if (cdr != null) return cdr.car;
        return null;
    }

    public ListStruct cddr() {

        if (cdr != null)
            return cdr.cdr;

        return null;
    }

    public Object cddar() {
        final ListStruct cddr = cddr();
        if (cddr != null) return cddr.car();
        return null;
    }

    public ListStruct cdddr() {

        if (cdr != null && cdr.cdr != null)
            return cdr.cdr.cdr;

        return null;
    }

    public Object cdddar() {
        final ListStruct cdddr = cdddr();
        if (cdddr != null) return cdddr.car();
        return null;
    }

    /**
     * return number of elements in the list
     */
    public int size() {
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
                if (isEmptyList) return false;
                return null != head; // && null != head.car();
            }

            @Override
            public Object next() {
                final Object car = head.car();
                head = head.cdr();
                return car;
            }

            @Override
            public void remove() {
                throw new RuntimeException("unbindLocal() not implemented");
            }
        };
    }
}
