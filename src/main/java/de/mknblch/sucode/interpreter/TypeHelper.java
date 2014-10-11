package de.mknblch.sucode.interpreter;

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

    public Boolean asBoolean(Object o) {
        return o != null;
    }
}
