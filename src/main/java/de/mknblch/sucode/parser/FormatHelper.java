package de.mknblch.sucode.parser;

import de.mknblch.sucode.parser.structs.Atom;
import de.mknblch.sucode.parser.structs.ConstStruct;
import de.mknblch.sucode.parser.structs.ListStruct;
import de.mknblch.sucode.parser.structs.SymbolStruct;

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
            case CONST:
                return String.valueOf(((ConstStruct) atom).value);
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
        if (!(obj instanceof Atom)) {
            return String.valueOf(obj);
        }
        final Atom atom = (Atom) obj;

        switch (atom.getType()) {

            case SYMBOL:
                return String.format("SYM:%s", ((SymbolStruct) atom).literal);
            case CONST:
                return String.format("C%s:%2$s", ((ConstStruct) atom).type.name(), String.valueOf(((ConstStruct) atom).value));
            case LIST:
                final ListStruct listStruct = (ListStruct) atom;
                final StringBuffer buffer = new StringBuffer();
                for (Object element : listStruct) {
                    if (buffer.length() > 0) buffer.append(", ");
                    buffer.append(formatAtom(element));
                }
                return String.format("[%s]", buffer.toString());
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
            case CONST:
                return String.format("C%s", ((ConstStruct) atom).type.name());
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
            case CONST:
                return String.format("%s", String.valueOf(((ConstStruct) atom).value));
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