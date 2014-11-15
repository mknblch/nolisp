package de.mknblch.nolisp.core.common;

import de.mknblch.nolisp.core.interpreter.structs.ListStruct;
import de.mknblch.nolisp.core.interpreter.structs.SymbolStruct;
import de.mknblch.nolisp.core.interpreter.structs.forms.Form;
import de.mknblch.nolisp.core.interpreter.structs.forms.LambdaForm;
import de.mknblch.nolisp.core.interpreter.structs.forms.SpecialForm;
import de.mknblch.nolisp.core.interpreter.EvaluationException;

public class Expectations {

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
        if (!Converter.isSymbolWithLiteral(o, "quote")) {
            throw new EvaluationException(
                    String.format("Expected QUOTE:SYM but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectQuotedList(Object o) throws EvaluationException {
        if (!(o instanceof ListStruct)) {
            throw new EvaluationException(
                    String.format("Expected QUOTED LIST but was: %s", FormatHelper.formatAtom(o)));
        }
        final ListStruct q = (ListStruct) o;
        if (!Converter.isSymbolWithLiteral(q.car(), "quote")) {
            throw new EvaluationException(
                    String.format("Expected QUOTED LIST but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectCdr(Object o) throws EvaluationException {
        if (null == o ||
                !(o instanceof ListStruct) ||
                null == ((ListStruct) o).cdr()) {
            throw new EvaluationException(
                    String.format("Expected LIST with rest"));
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