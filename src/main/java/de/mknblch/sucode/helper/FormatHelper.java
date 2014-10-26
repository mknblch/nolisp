package de.mknblch.sucode.helper;

import de.mknblch.sucode.ast.Atom;
import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.ast.forms.Function;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.ast.forms.MacroForm;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;

import java.util.List;
import java.util.Set;

public class FormatHelper {

    public static String formatContext(Context context, boolean addForms) {
        final Set<String> keys = context.keySetGlobal();
        final StringBuffer sb = new StringBuffer();
        for (String key : keys) {
            try {
                final Object obj = context.get(key);
                if (!addForms && obj instanceof Atom && ((Atom) obj).getType() == Atom.Type.FORM) continue;
                if (sb.length() > 0) sb.append(", ");
                sb.append(key).append(" => ").append(formatPretty(obj));
            } catch (EvaluationException e) {
                e.printStackTrace();
            }
        }
        return sb.toString();
    }

    public static String formatAsSExpression(ListStruct listStruct) {
        return String.format("(%s %s)",
                listStruct.car() instanceof ListStruct ? formatAsSExpression((ListStruct) listStruct.car()) : formatPretty(listStruct.car()),
                (listStruct.cdr() == null ? "nil" : formatAsSExpression(listStruct.cdr())));

    }

    public static String formatPretty(Object obj) {

        if (null == obj) {
            return "nil";
        }
        //special case List<Object>
        if(obj instanceof List) {
            final StringBuffer sb = new StringBuffer();
            final List list = (List) obj;
            for (Object o : list) {
                if (sb.length() > 0) sb.append(" ");
                sb.append(formatPretty(o));
            }
            return String.format("L[ %s ]", sb.toString());
        }
        // return Non-Atoms
        if (!(obj instanceof Atom)) {
            return String.valueOf(obj);
        }
        final Atom atom = (Atom) obj;

        switch (atom.getType()) {
            case LIST:
                final StringBuffer sb = new StringBuffer();
                ListStruct temp = (ListStruct) (ListStruct) atom;
                do {
                    if (sb.length() > 0) sb.append(" ");
                    sb.append(formatPretty(temp.car()));
                    temp = temp.cdr();
                } while (temp != null);

                return String.format("( %s )", sb.toString());
            case LAMBDA:
                final LambdaForm lambda = (LambdaForm) atom;
                return String.format("#<LAMBDA> (%s) %s", formatSymbols(lambda.getSymbols()), formatPretty(lambda.getForm()));
            case MACRO:
                final MacroForm macro = (MacroForm) atom;
                return String.format("#<MACRO %s> (%s) %s", macro.getSymbol(), formatSymbols(macro.getFormSymbols()), formatPretty(macro.getForm()));
            case SYMBOL:
                return String.format("%s", ((SymbolStruct) atom).literal);
            case FORM:
                return String.format("#<BUILTIN %s>", ((Function) atom).getSymbol());
        }
        throw new IllegalArgumentException(String.format("%s:UNKNOWN_ATOM", atom));
    }

    private static String formatSymbols(List<String> symbols) {
        final StringBuffer sb = new StringBuffer();
        for (String symbol : symbols) {
            if (sb.length() > 0) sb.append(" ");
            sb.append(symbol);
        }
        return sb.toString();
    }

    public static String formatAtom(Object obj) {

        if (null == obj) {
            return "null";
        }
        if (!(obj instanceof Atom)) {
            return String.format("%s:%s", String.valueOf(obj), obj.getClass().getSimpleName());
        }
        final Atom atom = (Atom) obj;

        switch (atom.getType()) {

            case SYMBOL:
                return String.format("%s:SYM", ((SymbolStruct) atom).literal);
            case LIST:
                final ListStruct listStruct = (ListStruct) atom;
                final StringBuffer buffer = new StringBuffer();
                for (Object element : listStruct) {
                    if (buffer.length() > 0) buffer.append(", ");
                    buffer.append(formatAtom(element));
                }
                return String.format("[ %s ]", buffer.toString());
            case LAMBDA:
                final LambdaForm lambda = (LambdaForm) atom;
                return String.format("#<LAMBDA> (%s) %s", formatSymbols(lambda.getSymbols()), formatAtom(lambda.getForm()));
            case MACRO:
                final MacroForm macro = (MacroForm) atom;
                return String.format("#<MACRO %s> (%s) %s", macro.getSymbol(), formatSymbols(macro.getFormSymbols()), formatPretty(macro.getForm()));
            case FORM:
                return String.format("#<BUILTIN %s>", ((Function) atom).getSymbol());
        }

        throw new IllegalArgumentException(String.format("%s:UNKNOWN_ATOM", atom));
    }
}