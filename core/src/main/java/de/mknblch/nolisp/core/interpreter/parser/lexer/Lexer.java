package de.mknblch.nolisp.core.interpreter.parser.lexer;

/**
 * basic lisp lexer.
 *
 */
public class Lexer extends StringCutter {

    public static final char[] IGNORE_CHARS = new char[]{' ', '\t', '\r'};
    public static final char[] NEWLINE_CHARS = new char[]{'\n'};
    public static final char[] SPECIAL_TOKEN_CHARS = new char[]{'(', ')', '\'', '#', '`', ',', '@'};

    public static final String INT_REGEX = "^\\-?[0-9]+$";
    public static final String REAL_REGEX = "^\\-?[0-9]+\\.[0-9]+$";
    public static final String NIL_REGEX = "^(nil)|(NIL)|(null)|(NULL)$";
    public static final String TRUE_REGEX = "^(t)|(T)|(true)|(TRUE)$";
    public static final String FALSE_REGEX = "^(false)|(FALSE)$";

    public Lexer() {
    }

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
            case '(':
                return new Token(Token.Type.LIST_BEGIN, "(", "(");
            case ')':
                return new Token(Token.Type.LIST_END, ")", ")");
            case '\'':
                return new Token(Token.Type.QUOTE, "'", "'");
            case '#':
                return new Token(Token.Type.SHARP, "#", "#");
            case '`':
                return new Token(Token.Type.BACKQUOTE, "`", "`");
            case ',':
                return new Token(Token.Type.COMMA, ",", ",");
            case '@':
                return new Token(Token.Type.SPLICE, "@", "@");
            case ';':
                final String comment = tokenizeComment();
                return new Token(Token.Type.LINE_COMMENT, comment, comment);
            case '"':
                final String str = tokenizeString();
                if (null == str) {
                    throw new LexerException(String.format("Parsing STRING failed - arg was null", str));
                }
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
        final StringBuffer buffer = new StringBuffer();
        while (true) {
            if(!hasNext()) {
                throw new LexerException("Premature end of string.");
            }
            final char c = popChar();
            if ('\\' == c) {
                final char n = popChar();
                switch (n) {
                    case '"': buffer.append("\""); break;
                    case 't': buffer.append("\t"); break;
                    case 'n': buffer.append("\n"); break;
                    case 'r': buffer.append("\r"); break;
                    case '\\': buffer.append("\\"); break;
                    default: throw new LexerException(String.format("Invalid escape sequence '\\%c'", n));
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

        if (literal.matches(INT_REGEX)) {
            try {
                return new Token(Token.Type.CONST, literal, Integer.parseInt(literal));
            } catch (Exception e) {
                throw new LexerException(String.format("Parsing '%s' to INT failed", literal), e.getCause());
            }
        } else if (literal.matches(REAL_REGEX)) {
            try {
                return new Token(Token.Type.CONST, literal, Double.parseDouble(literal));
            } catch (Exception e) {
                throw new LexerException(String.format("Parsing '%s' to REAL failed", literal), e.getCause());
            }
        } else if (literal.matches(NIL_REGEX)) {
            return new Token(Token.Type.CONST, "nil", null);
        } else if (literal.matches(TRUE_REGEX)) {
            return new Token(Token.Type.CONST, "true", Boolean.TRUE);
        } else if (literal.matches(FALSE_REGEX)) {
            return new Token(Token.Type.CONST, "false", Boolean.FALSE);
        } else {
            return new Token(Token.Type.SYMBOL, literal, literal);
        }
    }

}
