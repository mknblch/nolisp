package de.mknblch.sucode.parser.structs;

/**
 * Created by pexx on 03.10.2014.
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

    public void cons (Atom cons) {
        last().cdr = new ListStruct(cons);
    }

    public void addCons(Atom atom) {

        if (null == car) {
            car = atom;
        } else
            cons(atom);
    }

    public boolean hasSuccessor() {
        return null != cdr;
    }

    public Atom car() {
        return car;
    }

    public ListStruct cdr() {
        return cdr;
    }
}
