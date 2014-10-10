package de.mknblch.sucode.evaluator;

import de.mknblch.sucode.parser.structs.*;

import java.util.ArrayList;
import java.util.List;

/**
* Created by mknblch on 05.10.2014.
*/
public class Evaluator {

    public List<Object> evaluate (ListStruct program) throws EvaluationException {

        final ArrayList<Object> ret = new ArrayList<Object>();

//        do {
            ret.add(eval(program.car()));
//        } while (program.hasSuccessor());

        return ret;
    }

    private Object eval(Object obj) throws EvaluationException {

        if (obj instanceof Atom) {
            final Atom atom = (Atom) obj;
            switch (atom.getType()) {

                case SYMBOL:
                    return evalSymbol((SymbolStruct) atom);
                case CONST:
                    return ((ConstStruct) atom).value;
                case LIST:
                    return evalList((ListStruct) atom);
            }
            throw new EvaluationException("Unknown Atom: " + atom.getType());
        }
        return obj;
    }

    private Object evalList(ListStruct atom) throws EvaluationException {

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
