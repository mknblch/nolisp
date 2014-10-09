package de.mknblch.sucode.parser.structs;

/**
 * Created by mknblch on 03.10.2014.
 */
public class ListStruct implements Atom {

    private Atom car = null;
    private ListStruct cdr = null;

    public ListStruct() {}


    public ListStruct(Atom car) {
        this.car = car;
    }

    @Override
    public Type getType() {
        return Type.LIST;
    }

    /**
     * retrieve last element of the list
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
    public void add(Atom atom) {

        if (null == car) {
            car = atom;
        } else
            cons(atom);
    }

    /**
     * determine if this element has a successor
     * @return
     */
    public boolean hasSuccessor() {
        return null != cdr;
    }

    /**
     * get current list element
     * @return
     */
    public Atom car() {
        return car;
    }

    /**
     * get rest list
     * @return
     */
    public ListStruct cdr() {
        return cdr;
    }

    /**
     * add to end
     * @param cons
     */
    protected void cons (Atom cons) {
        last().cdr = new ListStruct(cons);
    }
}
