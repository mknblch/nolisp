package de.mknblch.nolisp.helper;

import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.ast.SymbolStruct;
import de.mknblch.nolisp.ast.forms.Form;
import de.mknblch.nolisp.ast.forms.Function;
import de.mknblch.nolisp.ast.forms.LambdaForm;
import de.mknblch.nolisp.interpreter.EvaluationException;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mknblch
 */
public class TypeHelper {

    public static Integer asInt(Object o) throws EvaluationException {
        if (isInt(o)) {
            return (Integer) o;
        }
        if (isReal(o)) {
            return ((Double) o).intValue();
        }
        throw new EvaluationException("Illegal INT cast.");
    }

    public static boolean isInt(Object o) {
        return o instanceof Integer;
    }

    public static Double asReal(Object o) throws EvaluationException {
        if (isInt(o)) {
            return (double) (Integer) o;
        }
        if (isReal(o)) {
            return ((Double) o);
        }
        throw new EvaluationException("Illegal REAL cast.");
    }

    public static boolean isReal(Object o) {
        return o instanceof Double;
    }

    public static String asString(Object o) throws EvaluationException {
        Expectations.expectString(o);
        return (String) o;
    }

    public static boolean isString(Object o) {
        return o instanceof String;
    }

    public static boolean isSymbol(Object o) {
        return o instanceof SymbolStruct;
    }

    public static boolean isSymbolWithLiteral(Object o, String literal) {
        return isSymbol(o) && literal.equals(((SymbolStruct) o).literal);
    }

    public static ListStruct asList(Object o) throws EvaluationException {
        Expectations.expectList(o);
        return (ListStruct) o;
    }

    public static boolean isList(Object o) {
        return o instanceof ListStruct;
    }

    public static String symbolLiteral(Object o) throws EvaluationException {
        Expectations.expectSymbol(o);
        return ((SymbolStruct) o).literal;
    }

    public static LambdaForm asLambda(Object o) throws EvaluationException {
        Expectations.expectLambda(o);
        return ((LambdaForm) o);
    }

    public static boolean isLambda(Object o) {
        return o instanceof LambdaForm;
    }

    public static Form asForm(Object o) throws EvaluationException {
        Expectations.expectForm(o);
        return ((Form) o);
    }

    public static boolean isForm(Object o) {
        return o instanceof Form;
    }

    public static Function asFunction(Object o) throws EvaluationException {
        Expectations.expectFunction(o);
        return ((Function) o);
    }

    public static boolean isFunction(Object o) {
        return o instanceof Function;
    }

    public static Boolean asBoolean(Object o) {
        if (null == o) {
            return false;
        }
        // TODO review
        return !Boolean.FALSE.equals(o);
    }

    public static boolean isBoolean(Object o) {
        return Boolean.FALSE.equals(o) || Boolean.TRUE.equals(o);
    }

    public static List<String> symbolList(Object o) throws EvaluationException {
        Expectations.expectList(o);
        final ArrayList<String> flat = new ArrayList<String>();
        final ListStruct listStruct = (ListStruct) o;
        for (Object t : listStruct) {
            flat.add(symbolLiteral(t));
        }
        return flat;
    }

    public static List<Object> asJavaList(ListStruct listStruct) {
        final List<Object> flat = new ArrayList<Object>();
        ListStruct temp = listStruct;
        while (temp != null) {
            flat.add(temp.car());
            temp = temp.cdr();
        }
        return flat;
    }

}
