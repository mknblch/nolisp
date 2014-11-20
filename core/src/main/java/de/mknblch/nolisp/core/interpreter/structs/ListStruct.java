package de.mknblch.nolisp.core.interpreter.structs;

import java.util.Iterator;

/**
 * custom linked list impl.
 *
 * @author mknblch
 */
public class ListStruct implements Atom, Iterable {

    private boolean isEmptyList = true;
    private Object car = null;
    private ListStruct cdr = null;

    public ListStruct(Object car, Object... rest) {
        isEmptyList = false;
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

    public boolean isEmpty() {
        return isEmptyList;
    }

    /**
     * add element to the list if:
     * the list has a successor  the element is appended to the end.
     * if not
     *
     * @param atom
     */
    private void append(Object atom) {

        if (cdr == null) cdr = new ListStruct(atom);
        else last().cdr = new ListStruct(atom);
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

    public Object nth(int n) {
        ListStruct temp = this;
        for (int i = 0; i < n; i++) {
            if (null == temp.cdr) return null;
            temp = temp.cdr;
        }
        return temp.car();
    }

    public Object nthcdr(int n) {
        ListStruct temp = this;
        for (int i = 0; i < n; i++) {
            if (null == temp.cdr) return null;
            temp = temp.cdr;
        }
        return temp;
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

    public Object cdar() {
        if (cdr != null) return cdr.car;
        return null;
    }

    public ListStruct cddr() {
        if (cdr != null) {
            return cdr.cdr;
        }
        return null;
    }

    public Object cddar() {
        final ListStruct cddr = cddr();
        if (cddr != null) return cddr.car();
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

    public ListStruct setCar(Object o) {
        isEmptyList = false;
        car = o;
        return this;
    }

    public ListStruct setCdr(ListStruct cdr) {
        this.cdr = cdr;
        return this;
    }

    public ListStruct attach(ListStruct cdr) {
        isEmptyList = false;
        last().setCdr(cdr);
        return this;
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
                return !isEmptyList && null != head;
            }

            @Override
            public Object next() {
                final Object car = head.car();
                head = head.cdr();
                return car;
            }

            @Override
            public void remove() {
                throw new RuntimeException("not implemented");
            }
        };
    }

    /**
     * container iterator
     */
    public Iterator<ListStruct> containerIterator() {

        return new Iterator<ListStruct>() {
            private ListStruct head = ListStruct.this;

            @Override
            public boolean hasNext() {
                return null != head;
            }

            @Override
            public ListStruct next() {
                final ListStruct temp = head;
                head = head.cdr();
                return temp;
            }

            @Override
            public void remove() {
                throw new RuntimeException("not implemented");
            }
        };
    }
}
