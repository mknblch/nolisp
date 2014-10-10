package de.mknblch.sucode.evaluator;

import de.mknblch.sucode.parser.structs.*;

import java.util.ArrayList;
import java.util.List;

/**
* Created by mknblch on 05.10.2014.
*/
public class Evaluator {

    public List<Object> evaluate (ListStruct program, Environment environment) throws EvaluationException {

        final ArrayList<Object> ret = new ArrayList<Object>();

//        do {
            ret.add(eval(program.car(), environment));
//        } while (program.hasSuccessor());

        return ret;
    }

    private Object eval(Object obj, Environment environment) throws EvaluationException {

        if (obj instanceof Atom) {
            final Atom atom = (Atom) obj;
            switch (atom.getType()) {

                case SYMBOL:
                    return evalSymbol((SymbolStruct) atom, environment);
                case LIST:
                    return evalList((ListStruct) atom, environment);
                case CONST:
                    return ((ConstStruct) atom).value;
            }
            throw new EvaluationException("Unknown Atom: " + atom.getType());
        }
        return obj;
    }

    private Object evalList(ListStruct atom, Environment environment) throws EvaluationException {

        final Object function = eval(atom.car(), environment);

        return evalFunction(function, atom.cdr(), environment);
    }

    private Object evalFunction(Object function, ListStruct args, Environment environment) throws EvaluationException {

        if ("+".equals(function)) {
            int x = 0;
            for (Object o : args) {
                x += (Integer) eval(o, environment);
            }
            return x;
        }

        return null;
    }

    private Object evalSymbol(SymbolStruct atom, Environment environment) {
        return atom.literal;
    }

}
