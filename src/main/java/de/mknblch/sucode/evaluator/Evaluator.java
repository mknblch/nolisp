package de.mknblch.sucode.evaluator;

import de.mknblch.sucode.parser.structs.Atom;
import de.mknblch.sucode.parser.Program;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by mknblch on 05.10.2014.
 */
public class Evaluator {


    public List<Object> evaluate(Program program) {

        final ArrayList<Object> retVal = new ArrayList<Object>(program.size());

        for (int i = 0; i < program.size(); i++) {
            retVal.add(eval(program.get(i)));
        }
        return retVal;
    }

    private Object eval (Atom struct) {



        return null;
    }
}
