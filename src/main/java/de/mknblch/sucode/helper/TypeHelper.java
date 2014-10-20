package de.mknblch.sucode.helper;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.interpreter.EvaluationException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by mknblch on 11.10.2014.
 */
public class TypeHelper {

    public static Integer asInt(Object o) throws EvaluationException {
        if (o instanceof Integer) {
            return (Integer) o;
        }
        if (o instanceof Double) {
            return ((Double) o).intValue();
        }
        throw new EvaluationException("Illegal INT cast.");
    }

    public static Double asReal(Object o) throws EvaluationException {
        if (o instanceof Integer) {
            return (double) (Integer) o;
        }
        if (o instanceof Double) {
            return ((Double) o);
        }
        throw new EvaluationException("Illegal REAL cast.");
    }

    public static String asString(Object o) throws EvaluationException {
        expectString(o);
        return (String) o;
    }

    public static String symbolLiteral(Object o) throws EvaluationException {
        expectSymbol(o);
        return ((SymbolStruct) o).literal;
    }

    public static Boolean asBoolean(Object o) {
        if (null == o) {
            return false;
        }
        // TODO review
        return !Boolean.FALSE.equals(o);
    }

    public static List<String> symbolList(Object o) throws EvaluationException {
        expectList(o);
        final ArrayList<String> flat = new ArrayList<String>();
        final ListStruct listStruct = (ListStruct) o;
        for (Object t : listStruct) {
            flat.add(symbolLiteral(t));
        }
        return flat;
    }

    public static void expectList(Object o) throws EvaluationException {
        if (!(o instanceof ListStruct)) {
            throw new EvaluationException(
                    String.format("Expected LIST but was: %s", o != null ? o.getClass().getSimpleName() : "null"));
        }
    }

    public static void expectSymbol(Object o) throws EvaluationException {
        if (!(o instanceof SymbolStruct)) {
            throw new EvaluationException(
                    String.format("Expected SYMBOL but was: %s", o != null ? o.getClass().getSimpleName() : "null"));
        }
    }

    public static void expectString(Object o) throws EvaluationException {
        if (!(o instanceof String)) {
            throw new EvaluationException(
                    String.format("Expected STRING but was: %s", o != null ? o.getClass().getSimpleName() : "null"));
        }
    }

    public static void expectNumber(Object o) throws EvaluationException {
        if (!(o instanceof Double || o instanceof Integer)) {
            throw new EvaluationException(
                    String.format("Expected NUMBER but was: %s", o != null ? o.getClass().getSimpleName() : "null"));
        }
    }

    public static void expectInt(Object o) throws EvaluationException {
        if (!(o instanceof Integer)) {
            throw new EvaluationException(
                    String.format("Expected INT but was: %s", o != null ? o.getClass().getSimpleName() : "null"));
        }
    }

    public static void expectReal(Object o) throws EvaluationException {
        if (!(o instanceof Double)) {
            throw new EvaluationException(
                    String.format("Expected REAL but was: %s", o != null ? o.getClass().getSimpleName() : "null"));
        }
    }
}
