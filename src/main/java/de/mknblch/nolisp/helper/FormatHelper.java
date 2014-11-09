package de.mknblch.nolisp.helper;

import de.mknblch.nolisp.ast.Atom;
import de.mknblch.nolisp.ast.ListStruct;
import de.mknblch.nolisp.ast.SymbolStruct;
import de.mknblch.nolisp.ast.forms.LambdaForm;
import de.mknblch.nolisp.ast.forms.MacroForm;
import de.mknblch.nolisp.func.BuiltIn;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;

import java.util.List;
import java.util.Set;

public class FormatHelper {

    public static String formatContext(Context context, boolean addBuiltInForms) {
        final Set<String> keys = context.keySetGlobal();
        final StringBuffer sb = new StringBuffer();
        for (String key : keys) {
            try {
                final Object obj = context.get(key);
                if (!addBuiltInForms && obj instanceof Atom && ((Atom) obj).getType() == Atom.Type.BUILTIN) continue;
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
                return String.format("#<LAMBDA> (%s) %s", formatSymbols(lambda.getArgumentSymbols()), formatPretty(lambda.getForm()));
            case MACRO:
                final MacroForm macro = (MacroForm) atom;
                return String.format("#<MACRO> (%s) %s", formatSymbols(macro.getArgumentSymbols()), formatPretty(macro.getForms()));
            case SYMBOL:
                return String.format("%s", ((SymbolStruct) atom).literal);
            case BUILTIN:
                return String.format("#<BUILTIN %s>", ((BuiltIn) atom).getSymbol());
        }
        throw new IllegalArgumentException(String.format("%s:UNKNOWN", atom));
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
                return String.format("#<LAMBDA> (%s) %s", formatSymbols(lambda.getArgumentSymbols()), formatAtom(lambda.getForm()));
            case MACRO:
                final MacroForm macro = (MacroForm) atom;
                return String.format("#<MACRO> (%s) %s", formatSymbols(macro.getArgumentSymbols()), formatPretty(macro.getForms()));
            case BUILTIN:
                return "#<BUILTIN>";
        }

        throw new IllegalArgumentException(String.format("%s:UNKNOWN_ATOM", atom));
    }
}