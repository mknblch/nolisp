package de.mknblch.nolisp.core.common;

import de.mknblch.nolisp.core.interpreter.EvaluationException;
import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.Form;
import de.mknblch.nolisp.core.interpreter.structs.forms.LambdaForm;

import java.lang.reflect.Type;
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

    public static boolean isObjectType(Object o, String type) throws ClassNotFoundException {

        final Class<?> typeClass = Class.forName(type);

        return typeClass.isAssignableFrom(o.getClass());
    }

    public static boolean isPrimitiveObjectType(Object o, String type) throws ClassNotFoundException {

        final Class<?> typeClass = Class.forName("java.lang.".concat(type));

        return typeClass.isAssignableFrom(o.getClass());
    }

    public static <T> T castToType (Object o, String type) throws ClassNotFoundException {
        final Class<?> typeClass = Class.forName(type);
        return (T) typeClass.cast(o);
    }

    public static Exception asException(Object o) throws ClassNotFoundException, EvaluationException {
        if(isPrimitiveObjectType(o, "Exception")) return castToType(o, "java.lang.Exception");
        throw new EvaluationException("Illegal Exception cast.");
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

    public static boolean isListWithSymbolHead(Object o, String symbolLiteral) {
        if (!isList(o)) return false;
        final Object head = ((ListStruct) o).car();
        return isSymbol(head) && symbolLiteral.equals(((SymbolStruct) head).literal);
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

    public static Boolean asBoolean(Object o) {
        return null != o && !Boolean.FALSE.equals(o);
    }

    public static boolean isBoolean(Object o) {
        return Boolean.FALSE.equals(o) || Boolean.TRUE.equals(o);
    }

    public static List<String> symbolList(Object o) throws EvaluationException {
        Expectations.expectList(o);
        final ArrayList<String> flat = new ArrayList<String>();
        final ListStruct listStruct = (ListStruct) o;
        for (Object t : listStruct) {
            if (null == t) {
                continue;
            }

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
