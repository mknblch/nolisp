package de.mknblch.sucode.evaluator;

import de.mknblch.sucode.parser.structs.*;
import de.mknblch.sucode.parser.Program;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by mknblch on 05.10.2014.
 */
public class Evaluator {

    public List<Object> evaluate (Program program) {

        final ArrayList<Object> ret = new ArrayList<Object>();

//        do {
            ret.add(eval(program.car()));
//        } while (program.hasSuccessor());

        return ret;
    }

    private Object eval(Object obj) {

        if (obj instanceof Atom) {
            final Atom atom = (Atom) obj;
            switch (atom.getType()) {

                case SYMBOL:
                    return evalSymbol((SymbolStruct) atom);
                case INT:
                    return ((IntStruct) atom).intValue;
                case REAL:
                    return ((RealStruct) atom).realValue;
                case STRING:
                    return ((StringStruct) atom).value;
                case END:
                    // oO
                    break;
                case QUOTED_LIST:
                    break;
                case LIST:
                    return evalList((ListStruct) atom);

            }
        }
        return obj;
    }

    private Object evalList(ListStruct atom) {

        final Object function = eval(atom.car());
        final ArrayList<Object> args = new ArrayList<Object>();

        ListStruct rest = atom.cdr();
        do {
            args.add(eval(rest.car()));
            rest = rest.cdr();
        } while (null != rest);


        return evalFunction(function, args);
    }

    private Object evalFunction(Object function, ArrayList<Object> args) {

        if ("+".equals(function)) {

            int x = 0;
            for (int i = 0; i < args.size(); i++) {
                x += (Integer)args.get(i);
            }
            return x;
        }

        return null;
    }

    private Object evalSymbol(SymbolStruct atom) {
        return atom.literal;
    }

}
