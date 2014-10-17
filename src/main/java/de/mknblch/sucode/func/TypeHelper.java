package de.mknblch.sucode.func;

import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.structs.ConstStruct;
import de.mknblch.sucode.structs.ListStruct;
import de.mknblch.sucode.structs.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

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
        throw new EvaluationException("Illegal INT cast.");
    }

    public static Double asReal(Object o) throws EvaluationException {
        if(o instanceof Integer) {
            return (double)(Integer) o;
        }
        if(o instanceof Double) {
            return ((Double) o);
        }
        throw new EvaluationException("Illegal REAL cast.");
    }

    public static String asString(Object o) throws EvaluationException {
        if(!(o instanceof String)) throw new EvaluationException("Illegal STRING cast.");
        return (String) o;
    }

    public static String symbolLiteral(Object o) throws EvaluationException {
        if(!(o instanceof SymbolStruct)) throw new EvaluationException("Illegal SYMBOL cast.");
        return ((SymbolStruct) o).literal;
    }

    public static Object constValue(Object o) throws EvaluationException {
        if(!(o instanceof ConstStruct)) throw new EvaluationException("Illegal CONST cast.");
        return ((ConstStruct) o).value;
    }
    public static Boolean asBoolean(Object o) {
        return o != null;
    }



    public static List<String> symbolList(Object o) throws EvaluationException {
        if(!(o instanceof ListStruct)) throw new EvaluationException("Illegal LIST cast.");
        final ArrayList<String> flat = new ArrayList<String>();
        final ListStruct listStruct = (ListStruct) o;
        try {
            for (Object t : listStruct) {
                flat.add(symbolLiteral(t));
            }
        } catch (ClassCastException cce) {
            throw new EvaluationException("Illegal CONST cast.");
        }
        return flat;
    }

    public static <T> List<T> convertConstListFlat(Object o) throws EvaluationException {
        if(!(o instanceof ListStruct)) throw new EvaluationException("Illegal LIST cast.");
        final ArrayList<T> flat = new ArrayList<T>();
        final ListStruct listStruct = (ListStruct) o;
        try {
            for (Object t : listStruct) {
                flat.add((T) constValue(t));
            }
        } catch (ClassCastException cce) {
            throw new EvaluationException("Illegal CONST cast.");
        }
        return flat;
    }

}
