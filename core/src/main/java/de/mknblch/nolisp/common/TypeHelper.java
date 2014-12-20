package de.mknblch.nolisp.common;

import de.mknblch.nolisp.datatypes.Atom;
import de.mknblch.nolisp.dialect.builtin.BuiltIn;
import de.mknblch.nolisp.features.macro.Macro;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SymbolStruct;
import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.features.lambda.Lambda;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mknblch
 */
public class TypeHelper {

    public static boolean isType(Object o, Class<?> type) {
        return null != o && type.isAssignableFrom(o.getClass());
    }

    public static boolean isBoolean(Object o) {
        return Boolean.FALSE.equals(o) || Boolean.TRUE.equals(o);
    }

    public static boolean isArray(Object o) {
        return o instanceof Object[];
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

    public static boolean isReal(Object o) {
        return o instanceof Double;
    }

    public static boolean isString(Object o) {
        return o instanceof String;
    }

    public static boolean isSymbol(Object o) {
        return o instanceof SymbolStruct;
    }

    public static Object isMacro(Object o) {
        return o instanceof Macro;
    }

    public static boolean isLambda(Object o) {
        return o instanceof Lambda;
    }

    public static boolean isForm(Object o) {
        return o instanceof Form;
    }

    public static boolean isSymbolWithLiteral(Object o, String literal) {
        return isSymbol(o) && literal.equals(((SymbolStruct) o).literal);
    }

    public static boolean isListWithSymbolHead(Object o, String symbolLiteral) {
        if (!isList(o)) return false;
        final Object head = ((ListStruct) o).car();
        return isSymbol(head) && symbolLiteral.equals(((SymbolStruct) head).literal);
    }

    public static boolean isList(Object o) {
        return o instanceof ListStruct;
    }

    public static boolean isEmptyList(Object o) {
        if (null == o) {
            return true;
        }
        if (!(o instanceof ListStruct)) {
            return false;
        } else {
            final ListStruct listStruct = (ListStruct) o;
            return listStruct.car() == null && !listStruct.hasSuccessor();
        }
    }

    public static Boolean asBoolean(Object o) {
        return null != o && !Boolean.FALSE.equals(o) && !isEmptyList(o);
    }

    public static Object[] asArray(Object o) throws EvaluationException {
        Expectations.expectArray(o);
        return (Object[]) o;
    }

    public static int asInt(Object o) throws EvaluationException {
        if (isInt(o)) {
            return (Integer) o;
        }
        if (isReal(o)) {
            return ((Double) o).intValue();
        }
        throw new EvaluationException("Illegal INT cast.");
    }

    public static double asReal(Object o) throws EvaluationException {
        if (isInt(o)) {
            return (double) (Integer) o;
        }
        if (isReal(o)) {
            return ((Double) o);
        }
        throw new EvaluationException("Illegal REAL cast.");
    }

    public static SymbolStruct asSymbol(Object o) throws EvaluationException {
        if (!isSymbol(o)) throw new EvaluationException("Illegal Symbol cast.");
        return (SymbolStruct) o;
    }

    public static ListStruct asList(Object o) throws EvaluationException {
        if (null == o) return null;
        Expectations.expectList(o);
        return (ListStruct) o;
    }

    public static Form asForm(Object o) throws EvaluationException {
        Expectations.expectForm(o);
        return ((Form) o);
    }

    public static Exception asException(Object o) throws ClassNotFoundException, EvaluationException {
        if (isPrimitiveObjectType(o, "Exception")) return castToType(o, "java.lang.Exception");
        throw new EvaluationException("Illegal Exception cast.");
    }

    public static String asString(Object o) throws EvaluationException {
        Expectations.expectString(o);
        return (String) o;
    }

    public static Lambda asLambda(Object o) throws EvaluationException {
        Expectations.expectLambda(o);
        return ((Lambda) o);
    }

    //////////////////////////////////////////////////////////////////////////////////

    public static Object[] convertListToArray(Object o) throws EvaluationException {
        if (null == o) return null;
        final ListStruct listStruct = asList(o);
        final int size = listStruct.size();
        final Object[] objects = new Object[size];
        int n = 0;
        for (Object obj : listStruct) {
            objects[n++] = obj;
        }
        return objects;
    }

    public static List<String> convertToSymbolList(Object o) throws EvaluationException {
        Expectations.expectList(o);
        final ArrayList<String> flat = new ArrayList<String>();
        final ListStruct listStruct = (ListStruct) o;
        for (Object t : listStruct) {
            if (null == t) {
                continue;
            }

            flat.add(getSymbolLiteral(t));
        }
        return flat;
    }

    public static ListStruct convertToListStruct(List<?> list) {
        final ListStruct ret = new ListStruct();
        for (Object object : list) {
            ret.add(object);
        }
        return ret;
    }

    public static ListStruct convertToListStruct(Object[] objects) {
        final ListStruct ret = new ListStruct();
        for (Object object : objects) {
            ret.add(object);
        }
        return ret;
    }

    public static String getSymbolLiteral(Object o) throws EvaluationException {
        Expectations.expectSymbol(o);
        return ((SymbolStruct) o).literal;
    }

    //////////////////////////////////////////////////////////////////////////////////

    public static <T> T castToType(Object o, String type) throws ClassNotFoundException {
        final Class<?> typeClass = Class.forName(type);
        return (T) typeClass.cast(o);
    }

    public static Object isAtom(Object o) {
        return o instanceof Atom;
    }

    public static boolean isBuiltIn(Object o) {
        return o instanceof BuiltIn;
    }
}
