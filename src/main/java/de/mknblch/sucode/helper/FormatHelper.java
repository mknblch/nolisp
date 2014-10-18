package de.mknblch.sucode.helper;

import de.mknblch.sucode.parser.ParserException;
import de.mknblch.sucode.ast.Atom;
import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;

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
        // return Non-Atoms
        if (!(obj instanceof Atom)) {
            return String.valueOf(obj);
        }
        final Atom atom = (Atom) obj;

        switch (atom.getType()) {
            case SYMBOL:
                return ((SymbolStruct) atom).literal;
            case LIST:
                final StringBuffer sb = new StringBuffer();
                ListStruct temp = (ListStruct) (ListStruct) atom;
                do {
                    if (sb.length() > 0) sb.append(" ");
                    sb.append(formatPretty(temp.car()));
                    temp = temp.cdr();
                } while (temp != null);

                return String.format("( %s )", sb.toString());
        }
        return atom.getType().name();
    }

    public static String formatAtom(Object obj) throws ParserException {

        // return Non-Atoms
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
                return String.format("[%s]", buffer.toString());
            case SPECIAL_FORM:
            case FORM:
                return String.format("#<%s>", atom.getType());
        }

        throw new ParserException("Unable to format " + atom);
    }


    public static String formatTypesOnly(Object obj) throws ParserException {

        // return Non-Atoms
        if (!(obj instanceof Atom)) {
            return "<Java>";
        }
        final Atom atom = (Atom) obj;

        switch (atom.getType()) {

            case SYMBOL:
                return "SYM";
            case LIST:
                ListStruct listStruct = (ListStruct) atom;
                final StringBuffer buffer = new StringBuffer();
                for (Object element : listStruct) {
                    if (buffer.length() > 0) buffer.append(", ");
                    buffer.append(formatTypesOnly(element));
                }
                return String.format("LIST:[%s]", buffer.toString());
        }

        throw new ParserException("Unable to format " + atom);
    }

    public static String formatValuesOnly(Object obj) throws ParserException {

        // return Non-Atoms
        if (!(obj instanceof Atom)) {
            return String.valueOf(obj);
        }
        final Atom atom = (Atom) obj;

        switch (atom.getType()) {

            case SYMBOL:
                return String.format("%s", ((SymbolStruct) atom).literal);
            case LIST:
                ListStruct listStruct = (ListStruct) atom;
                final StringBuffer buffer = new StringBuffer();
                for (Object element : listStruct) {
                    if (buffer.length() > 0) buffer.append(", ");
                    buffer.append(formatValuesOnly(element));
                }
                return String.format("[%s]", buffer.toString());
        }

        throw new ParserException("Unable to format " + atom);
    }

}