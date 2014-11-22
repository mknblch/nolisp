package de.mknblch.nolisp.core.interpreter.parser.lexer;

import de.mknblch.nolisp.core.interpreter.parser.lexer.rules.*;

import java.util.ArrayList;

/**
 * basic lisp lexer.
 */
public class Lexer extends StringCutter {

    private static final char[] IGNORE_CHARS = new char[]{' ', '\t', '\r'};
    private static final char[] NEWLINE_CHARS = new char[]{'\n'};
    private static final char[] SPECIAL_TOKEN_CHARS = new char[]{'(', ')', '{', '}', '[', ']'};

    // TODO refactor
    private final ArrayList<TokenDecisionRule> decisionRules = new ArrayList<TokenDecisionRule>() {{
        add(new NullRule());
        add(new IntRule());
        add(new RealRule());
        add(new BigIntegerRule());
        add(new BigDecimalRule());
    }};

    public void setCode(String code) {
        if (null == code) {
            throw new IllegalArgumentException("null not allowed");
        } else {
            setString(code.trim());
        }
    }

    /**
     * fetch next token and increment offset.
     *
     * @return next token
     */
    public Token next() throws LexerException {
        skipIgnorable();
        if (!hasNext()) return null;
        sync();
        return tokenize(popChar());
    }

    private Token tokenize(char c) throws LexerException {
        switch (c) {
            case '[':
                return new Token(Token.Type.ARRAY_BEGIN, "[", "[");
            case ']':
                return new Token(Token.Type.ARRAY_END, "]", "]");
            case '(':
                return new Token(Token.Type.LIST_BEGIN, "(", "(");
            case ')':
                return new Token(Token.Type.LIST_END, ")", ")");
            case '{':
                return new Token(Token.Type.LIST_BEGIN, "{", "{");
            case '}':
                return new Token(Token.Type.LIST_END, "}", "}");
            case '\'':
                return new Token(Token.Type.QUOTE, "'", "'");
            case '#':
                return new Token(Token.Type.SHARP, "#", "#");
            case '`':
                return new Token(Token.Type.BACKQUOTE, "`", "`");
            case ',':
                return new Token(Token.Type.COMMA, ",", ",");
            case '.':
                return new Token(Token.Type.SPLICE, ".", ".");
            case '@':
                return new Token(Token.Type.SPLICE, "@", "@");
            case ';':
                final String comment = tokenizeComment();
                return new Token(Token.Type.LINE_COMMENT, comment, comment);
            case '"':
                final String str = tokenizeString();
                return new Token(Token.Type.CONST, str, str);
            default:
                return decideConstType(tokenizeConst());
        }
    }

    private String tokenizeComment() {
        until(NEWLINE_CHARS);
        return getToken();
    }


    private String tokenizeConst() {
        until(IGNORE_CHARS, SPECIAL_TOKEN_CHARS, NEWLINE_CHARS);
        return getToken();
    }

    private String tokenizeString() throws LexerException {
        final StringBuilder buffer = new StringBuilder();
        while (true) {
            if (!hasNext()) {
                throw new LexerException("Premature end of string.");
            }
            final char c = popChar();
            if ('\\' == c) {
                final char n = popChar();
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

        sync();
        return buffer.toString();
    }

    /**
     * skip ignorable chars. e.g. whitespaces and newline
     */
    private void skipIgnorable() {
        skip(IGNORE_CHARS, NEWLINE_CHARS);
    }

    private Token decideConstType(String literal) throws LexerException {
        for (TokenDecisionRule decisionRule : decisionRules) {
            final Token token = decisionRule.token(literal);
            if (null != token) return token;
        }
        return new Token(Token.Type.SYMBOL, literal, literal);
    }

}
