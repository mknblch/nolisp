package de.mknblch.nolisp.parser.lexer;

import static de.mknblch.nolisp.parser.lexer.TokenHelper.*;

/**
 * basic lisp lexer.
 *
 */
public class Lexer extends StringRunner {

    public static final char[] IGNORE_CHARS = new char[]{' ', '\t', '\r'};
    public static final char[] NEWLINE_CHARS = new char[]{'\n'};
    public static final char[] SPECIAL_TOKEN_CHARS = new char[]{'(', ')', '\'', '#', '`', ',', '@'};
    public static final char[] DOUBLEQUOTE_CHARS = new char[]{'"'};

    public static final String INT_REGEX = "^\\-?[0-9]+$";
    public static final String REAL_REGEX = "^\\-?[0-9]+\\.[0-9]+$";
    public static final String NIL_REGEX = "^(nil)|(NIL)|(null)|(NULL)$";
    public static final String TRUE_REGEX = "^(t)|(T)|(true)|(TRUE)$";
    public static final String FALSE_REGEX = "^(false)|(FALSE)$";

    public Lexer() {
    }

    public Lexer(String code) {
        super();
        setCode(code);
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
        return tokenize(charAtOffset());
    }

    private Token tokenize(char c) throws LexerException {
        switch (c) {
            case '(':
                inc();
                return makeListBeginToken();
            case ')':
                inc();
                return makeListEndToken();
            case '\'':
                inc();
                return makeQuoteToken();
            case '#':
                inc();
                return makeSharpToken();
            case '`':
                inc();
                return makeBackquoteToken();
            case ',':
                inc();
                return makeCommaToken();
            case '@':
                inc();
                return makeSpliceToken();
            case ';':
                return makeCommentToken(tokenizeComment());
            case '"':
                return makeStringToken(tokenizeString());
            default:
                return decideConstType(tokenizeConst());
        }
    }

    private Token decideConstType(String literal) throws LexerException {

        if (literal.matches(INT_REGEX)) {
            return makeIntToken(literal);
        } else if (literal.matches(REAL_REGEX)) {
            return makeRealToken(literal);
        } else if (literal.matches(NIL_REGEX)) {
            return makeNilToken();
        } else if (literal.matches(TRUE_REGEX)) {
            return makeTrueToken();
        } else if (literal.matches(FALSE_REGEX)) {
            return makeFalseToken();
        } else {
            return makeSymbolToken(literal);
        }
    }


    private String tokenizeComment() {
        sync();
        until(NEWLINE_CHARS);
        return getToken();
    }

    private String tokenizeConst() {
        sync();
        until(IGNORE_CHARS, SPECIAL_TOKEN_CHARS, NEWLINE_CHARS);
        return getToken();
    }

    private String tokenizeString() throws LexerException {
        // store start index of string including " and inc offset
        sync();
        inc();
        until(DOUBLEQUOTE_CHARS);
        // inc offset and store end of string including "
        inc();
        // if the last increment grows offset above code.length throw an exception
        if (getOffset() > getStr().length()) {
            throw new LexerException("premature end of string.");
        }
        // TODO escapes
        final String token = getToken();
        return token; // code.substring(startIndex, endIndex);
    }

    /**
     * skip ignorable chars. e.g. whitespaces and newline
     */
    private void skipIgnorable() {
        skip(IGNORE_CHARS, NEWLINE_CHARS);
    }

}
