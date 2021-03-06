package de.mknblch.nolisp.common;

import de.mknblch.nolisp.datatypes.*;

import java.util.List;

public class FormatHelper {

    public static String formatAsSExpression(ListStruct listStruct) {
        return String.format("(%s %s)",
                listStruct.car() instanceof ListStruct ? formatAsSExpression((ListStruct) listStruct.car()) : formatPretty(listStruct.car()),
                (listStruct.cdr() == null ? "nil" : formatAsSExpression(listStruct.cdr())));

    }

    public static String formatPretty(Object obj) {

        if (null == obj) {
            return "nil";
        }
        //special case List
        if (obj instanceof List) {
            final StringBuilder sb = new StringBuilder();
            final List list = (List) obj;
            for (Object o : list) {
                if (sb.length() > 0) sb.append(" ");
                sb.append(formatPretty(o));
            }
            return String.format("L[ %s ]", sb.toString());
        }
        //special case Array
        if (obj instanceof Object[]) {
            final StringBuilder sb = new StringBuilder();
            final Object[] list = (Object[]) obj;
            for (Object o : list) {
                if (sb.length() > 0) sb.append(" ");
                sb.append(formatPretty(o));
            }
            return String.format("A[ %s ]", sb.toString());
        }
        if(obj instanceof Form) {
            return "#<FORM>";
        }
        if(obj instanceof SpecialForm) {
            return "#<SPECIAL>";
        }

        // return Non-Atoms
        if (!(obj instanceof Atom)) {
            return String.valueOf(obj);
        }

        final Atom atom = (Atom) obj;
        switch (atom.getType()) {
            case SYMBOL:
                return String.format("%s", ((SymbolStruct) atom).literal);
            case LIST:
                final StringBuilder sb = new StringBuilder();
                ListStruct temp = (ListStruct) obj;
                do {
                    if (sb.length() > 0) sb.append(" ");
                    sb.append(formatPretty(temp.car()));
                    temp = temp.cdr();
                } while (temp != null);

                return String.format("( %s )", sb.toString());
        }
        throw new IllegalArgumentException(String.format("%s:UNKNOWN", atom));
    }

    private static String formatSymbols(List<String> symbols) {
        final StringBuilder sb = new StringBuilder();
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
                final StringBuilder buffer = new StringBuilder();
                for (Object element : listStruct) {
                    if (buffer.length() > 0) buffer.append(", ");
                    buffer.append(formatAtom(element));
                }
                return String.format("( %s )", buffer.toString());
        }

        throw new IllegalArgumentException(String.format("%s:UNKNOWN_ATOM", atom));
    }
}