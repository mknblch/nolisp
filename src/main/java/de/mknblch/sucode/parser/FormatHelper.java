package de.mknblch.sucode.parser;

import de.mknblch.sucode.parser.structs.*;

public class FormatHelper {

    public static String formatAsSExpression(ListStruct listStruct) {
        return String.format("(%s %s)",
                listStruct.car() instanceof ListStruct ? formatAsSExpression((ListStruct) listStruct.car()) : formatPretty(listStruct.car()),
                (listStruct.cdr() == null ? "nil" : formatAsSExpression(listStruct.cdr())));

    }

    public static String formatPretty(Atom atom) {

        if (null == atom) {
            return "nil";
        }

        switch (atom.getType()) {

            case SYMBOL:
                return ((SymbolStruct) atom).literal;
            case INT:
                return String.valueOf(((IntStruct) atom).intValue);
            case REAL:
                return String.valueOf(((RealStruct) atom).realValue);
            case STRING:
                return ((SymbolStruct) atom).literal;
            case LIST:
            case QUOTED_LIST:
                final StringBuffer sb = new StringBuffer();
                ListStruct temp = (ListStruct) (ListStruct) atom;
                do {
                    if(sb.length() > 0) sb.append(" ");
                    sb.append(formatPretty(temp.car()));
                    temp = temp.cdr();
                } while (temp != null);

                return String.format("( %s )", sb.toString());
        }

        return atom.getType().name();
    }

}