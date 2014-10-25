package de.mknblch.sucode.helper;

import de.mknblch.sucode.ast.Atom;
import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.parser.ParserException;

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
                return String.format("#<LAMBDA> (%s) %s", formatLambdaSymbols(lambda.getSymbols()), formatPretty(lambda.getForm()));
            case FORM:
                return "#<FORM>";
            case SYMBOL:
                return String.format("%s", ((SymbolStruct) atom).literal);
        }
        return atom.getType().name();
    }

    private static String formatLambdaSymbols(List<String> symbols) {
        final StringBuffer sb = new StringBuffer();
        for (String symbol : symbols) {
            if (sb.length() > 0) sb.append(" ");
            sb.append(symbol);
        }
        return sb.toString();
    }
}