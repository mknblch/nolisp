package de.mknblch.sucode.interpreter.func;

import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.parser.structs.SymbolStruct;

/**
 * Created by mknblch on 11.10.2014.
 */
public class TypeHelper {

    public static Integer asInt(Object o) throws EvaluationException {
        if(o instanceof Integer) {
            return (Integer) o;
        }
        if(o instanceof Double) {
            return ((Double) o).intValue();
        }
        throw new EvaluationException("Invalid type cast.");
    }

    public static Double asReal(Object o) throws EvaluationException {
        if(o instanceof Integer) {
            return (double)(Integer) o;
        }
        if(o instanceof Double) {
            return ((Double) o);
        }
        throw new EvaluationException("Invalid type cast.");
    }

    public static String asString(Object o) throws EvaluationException {
        if(!(o instanceof String)) throw new EvaluationException("Invalid type cast.");
        return (String) o;
    }

    public static String symbolLiteral(Object o) throws EvaluationException {
        if(!(o instanceof SymbolStruct)) throw new EvaluationException("Invalid type cast.");
        return ((SymbolStruct) o).literal;
    }

    public Boolean asBoolean(Object o) {
        return o != null;
    }
}
