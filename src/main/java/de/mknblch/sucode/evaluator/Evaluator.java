package de.mknblch.sucode.evaluator;

import de.mknblch.sucode.parser.structs.Atom;
import de.mknblch.sucode.parser.structs.ConstStruct;
import de.mknblch.sucode.parser.structs.ListStruct;
import de.mknblch.sucode.parser.structs.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by mknblch on 05.10.2014.
 */
public class Evaluator {

    public List<Object> evaluate(ListStruct program, Environment environment) throws EvaluationException {

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

        final Object function = atom.car();

        if (!(function instanceof SymbolStruct)) {
            throw new EvaluationException("Unable to evaluate: " + String.valueOf(function));
        }

        return evalFunction((SymbolStruct) atom.car(), atom.cdr(), environment);
    }

    private Object evalFunction(SymbolStruct function, ListStruct args, Environment environment) throws EvaluationException {

        if ("quote".equals(function.literal)) {
            return args;
        }

        if ("+".equals(function.literal)) {
            int x = 0;
            for (Object o : args) {
                x += (Integer) eval(o, environment);
            }
            return x;
        }

        if ("print".equals(function.literal)) {
            for (Object o : args) {
                System.out.println(eval(o, environment));
            }
        }

        return null;
    }

    private Object evalSymbol(SymbolStruct atom, Environment environment) {

        return environment.get(atom.literal);
    }

}
