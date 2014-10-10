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
                case LIST:
                    return evalList((ListStruct) atom);
                case CONST:
                    return ((ConstStruct) atom).value;
            }
            throw new EvaluationException("Unknown Atom: " + atom.getType());
        }
        return obj;
    }

    private Object evalList(ListStruct atom) throws EvaluationException {

        final Object function = eval(atom.car());

        return evalFunction(function, atom.cdr());
    }

    private Object evalFunction(Object function, ListStruct cdr) throws EvaluationException {

        if ("+".equals(function)) {
            int x = 0;
            for (Object o : cdr) {
                x += (Integer) eval(o);
            }
            return x;
        }

        return null;
    }

    private Object evalSymbol(SymbolStruct atom) {
        return atom.literal;
    }

}
