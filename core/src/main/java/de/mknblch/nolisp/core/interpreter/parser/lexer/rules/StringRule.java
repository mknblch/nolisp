package de.mknblch.nolisp.core.interpreter.parser.lexer.rules;

import de.mknblch.nolisp.core.interpreter.parser.lexer.*;

/**
 * @author mknblch
 */
public class StringRule implements TokenDecisionRule {

    private static final char[] NEWLINE_CHARS = new char[]{'\n'};

    @Override
    public Token decide(char head, StringCutter cutter) throws LexerException {

        if ('"' != head) return null;
        final String literal = tokenizeString(cutter);
        return new Token(Token.Type.CONST, literal, literal);
    }

    private String tokenizeString(StringCutter cutter) throws LexerException {
        final StringBuilder buffer = new StringBuilder();
        while (true) {
            if (!cutter.hasNext()) {
                throw new LexerException("Premature end of string.");
            }
            final char c = cutter.popChar();
            if ('\\' == c) {
                final char n = cutter.popChar();
                switch (n) {
                    case '"':
                        buffer.append("\"");
                        break;
                    case 't':
                        buffer.append("\t");
                        break;
                    case 'n':
                        buffer.append("\n");
                        break;
                    case 'r':
                        buffer.append("\r");
                        break;
                    case '\\':
                        buffer.append("\\");
                        break;
                    default:
                        throw new LexerException(String.format("Invalid escape sequence '\\%c'", n));
                }
            } else if ('"' == c) {
                break;
            } else {
                buffer.append(c);
            }
        }

        cutter.sync();
        return buffer.toString();
    }

}
