package de.mknblch.nolisp.common;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.datatypes.SymbolStruct;
import de.mknblch.nolisp.interpreter.EvaluationException;

public class Expectations {

    public static <T extends Class<?>> void expect(Object o, T clazz) {

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


    public static void expectNull(Object o) throws EvaluationException {
        if (null != o) {
            throw new EvaluationException(
                    String.format("Expected NULL but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectNotNull(Object o) throws EvaluationException {
        if (null == o) {
            throw new EvaluationException("Expected NOT NULL but was: null");
        }
    }

    public static void expectQuote(Object o) throws EvaluationException {
        expectSymbolWithLiteral(o, "quote");
    }


    public static void expectSymbolWithLiteral(Object o, String literal) throws EvaluationException {
        if (!TypeHelper.isSymbolWithLiteral(o, literal)) {
            throw new EvaluationException(
                    String.format("Expected SYM<%s> but was: %s", literal, FormatHelper.formatAtom(o)));
        }
    }

    public static void expectQuotedList(Object o) throws EvaluationException {
        expectListWithSymbol(o, "quote");
    }

    public static void expectListWithSymbol(Object o, String head) throws EvaluationException {
        expectSymbolWithLiteral(TypeHelper.asList(o).car(), head);
    }

    public static void expectCdr(Object o) throws EvaluationException {
        if (null == o ||
                !(o instanceof ListStruct) ||
                null == ((ListStruct) o).cdr()) {
            throw new EvaluationException(
                    String.format("Expected LIST with rest"));
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
                    String.format("Expected SPECIAL FORM but was: %s", FormatHelper.formatAtom(o)));
        }
    }

    public static void expectArray(Object o) throws EvaluationException {
        if(!TypeHelper.isArray(o)) {
            throw new EvaluationException(String.format("Expected ARRAY but was: %s", FormatHelper.formatAtom(o)));
        }
    }
}