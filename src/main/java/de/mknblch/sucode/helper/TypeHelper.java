package de.mknblch.sucode.helper;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.ast.forms.SpecialForm;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.parser.Parser;

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
        expectString(o);
        return (String) o;
    }

    public static String symbolLiteral(Object o) throws EvaluationException {
        expectSymbol(o);
        return ((SymbolStruct) o).literal;
    }

    public static LambdaForm asLambda(Object o) throws EvaluationException {
        expectLambda(o);
        return ((LambdaForm) o);
    }

    public static Form asForm(Object o) throws EvaluationException {
        expectForm(o);
        return ((Form) o);
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
                    String.format("Expected LIST but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectSymbol(Object o) throws EvaluationException {
        if (!(o instanceof SymbolStruct)) {
            throw new EvaluationException(
                    String.format("Expected SYMBOL but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectString(Object o) throws EvaluationException {
        if (!(o instanceof String)) {
            throw new EvaluationException(
                    String.format("Expected STRING but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectNumber(Object o) throws EvaluationException {
        if (!(o instanceof Double || o instanceof Integer)) {
            throw new EvaluationException(
                    String.format("Expected NUMBER but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectInt(Object o) throws EvaluationException {
        if (!(o instanceof Integer)) {
            throw new EvaluationException(
                    String.format("Expected INT but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectReal(Object o) throws EvaluationException {
        if (!(o instanceof Double)) {
            throw new EvaluationException(
                    String.format("Expected REAL but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectQuote(Object o) throws EvaluationException {
        System.out.println("examining " + o + " against " + Parser.QUOTE_STRUCT);
        if (o != Parser.QUOTE_STRUCT) {
            throw new EvaluationException(
                    String.format("Expected QUOTE:SYM but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectQuotedList(Object o) throws EvaluationException {
        if (!(o instanceof ListStruct)) {
            throw new EvaluationException(
                    String.format("Expected LIST but was: %s", FormatHelper.formatAtom(o)));
        }
        expectQuote(((ListStruct)o).car());
    }

    public static void expectCdr(Object o) throws EvaluationException {
        if (null == o ||
                !(o instanceof ListStruct) ||
                null == ((ListStruct) o).cdr()) {
            throw new EvaluationException(
                    String.format("Expected LIST with rest"));
        }
    }

    public static void expectFunction(Object o) throws EvaluationException {
        if (!(o instanceof Function)) {
            throw new EvaluationException(
                    String.format("Expected FUNCTION but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectLambda(Object o) throws EvaluationException {
        if (!(o instanceof LambdaForm)) {
            throw new EvaluationException(
                    String.format("Expected LAMBDA but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectForm(Object o) throws EvaluationException {
        if (!(o instanceof Form)) {
            throw new EvaluationException(
                    String.format("Expected FORM but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectSpecialForm(Object o) throws EvaluationException {
        if (!(o instanceof SpecialForm)) {
            throw new EvaluationException(
                    String.format("Expected SPECIAL_FORM but was: %s", FormatHelper.formatAtom(o)));
        }
    }


}
