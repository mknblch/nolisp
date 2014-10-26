package de.mknblch.sucode.helper;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.interpreter.EvaluationException;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mknblch
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
        Expectations.expectString(o);
        return (String) o;
    }

    public static ListStruct asList(Object o) throws EvaluationException {
        Expectations.expectList(o);
        return (ListStruct) o;
    }

    public static String symbolLiteral(Object o) throws EvaluationException {
        Expectations.expectSymbol(o);
        return ((SymbolStruct) o).literal;
    }

    public static LambdaForm asLambda(Object o) throws EvaluationException {
        Expectations.expectLambda(o);
        return ((LambdaForm) o);
    }

    public static Form asForm(Object o) throws EvaluationException {
        Expectations.expectForm(o);
        return ((Form) o);
    }

    public static Function asFunction(Object o) throws EvaluationException {
        Expectations.expectFunction(o);
        return ((Function) o);
    }

    public static Boolean asBoolean(Object o) {
        if (null == o) {
            return false;
        }
        // TODO review
        return !Boolean.FALSE.equals(o);
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


}
