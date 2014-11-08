package de.mknblch.nolisp.parser.lexer;

import static de.mknblch.nolisp.parser.lexer.TokenHelper.*;

/**
 * basic lisp lexer.
 *
 */
public class Lexer {

    public static final char[] IGNORE_CHARS = new char[]{' ', '\t', '\r'};
    public static final char[] NEWLINE_CHARS = new char[]{'\n'};
    public static final char[] SPECIAL_TOKEN_CHARS = new char[]{'(', ')', '\'', '#', '`', ',', '@'};
    public static final char[] DOUBLEQUOTE_CHARS = new char[]{'"'};

    public static final String INT_REGEX = "^\\-?[0-9]+$";
    public static final String REAL_REGEX = "^\\-?[0-9]+\\.[0-9]+$";
    public static final String NIL_REGEX = "^(nil)|(NIL)|(null)|(NULL)$";
    public static final String TRUE_REGEX = "^(t)|(T)|(true)|(TRUE)$";
    public static final String FALSE_REGEX = "^(false)|(FALSE)$";

    private String code;
    private int offset = 0;

    public Lexer() {
    }

    public Lexer(String code) {
        setCode(code);
    }

    public void setCode(String code) {
        if (null == code) {
            throw new IllegalArgumentException("null not allowed");
        } else {
            this.code = code.trim();
        }
        reset();
    }

    /**
     * reset state
     */
    public void reset() {
        offset = 0;
    }

    public int getOffset() {
        return offset;
    }

    /**
     * determine if lexer has more tokens.
     *
     * @return true if tokens left
     */
    public boolean hasNext() {
        return offset < code.length();
    }


    /**
     * fetch next token and increment offset.
     *
     * @return next token
     */
    public Token next() throws LexerException {
        skipIgnorable();
        if (offset >= code.length()) return null;
        return tokenize(code.charAt(offset));
    }

    private Token tokenize(char c) throws LexerException {
        switch (c) {
            case '(':
                offset++;
                return makeListBeginToken();
            case ')':
                offset++;
                return makeListEndToken();
            case '\'':
                offset++;
                return makeQuoteToken();
            case '#':
                offset++;
                return makeSharpToken();
            case '`':
                offset++;
                return makeBackquoteToken();
            case ',':
                offset++;
                return makeCommaToken();
            case '@':
                offset++;
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
        int startIndex = offset;
        until(NEWLINE_CHARS);
        return code.substring(startIndex, offset);
    }

    private String tokenizeConst() {
        int startIndex = offset;
        until(IGNORE_CHARS, SPECIAL_TOKEN_CHARS, NEWLINE_CHARS);
        return code.substring(startIndex, offset);
    }

    private String tokenizeString() throws LexerException {
        // store start index of string including " and inc offset
        int startIndex = offset++;
        until(DOUBLEQUOTE_CHARS);
        // inc offset and store end of string including "
        int endIndex = ++offset;
        // if the last increment grows offset above code.length throw an exception
        if (offset > code.length()) {
            throw new LexerException(String.format("[%03d] premature end of string found.", startIndex));
        }
        // TODO escapes
        return code.substring(startIndex, endIndex);
    }

    /**
     * skip ignorable chars. e.g. whitespaces and newline
     */
    private void skipIgnorable() {
        skip(IGNORE_CHARS, NEWLINE_CHARS);
    }

    /**
     * increment offset until any char OTHER THEN charsToSkip is found
     *
     * @param charsToSkip these chars should be ignored
     */
    private void skip(char[]... charsToSkip) {
        for (int i = offset; i < code.length(); i++) {
            if (E(code.charAt(offset), charsToSkip)) {
                offset++;
            } else {
                return;
            }
        }
    }

    /**
     * increment offset until any of charsToStop is found
     *
     * @param charsToStop chars to search for
     */
    private void until(char[]... charsToStop) {
        for (int i = offset; i < code.length(); i++) {
            if (!E(code.charAt(offset), charsToStop)) {
                offset++;
            } else {
                return;
            }
        }
    }

    /**
     * determine if char a is element of c
     *
     * @param a single char for comparision
     * @param c set of chars
     * @return true if a is element of c. false otherwise
     */
    public static boolean E (char a, char[]... c) {
        for (char[] chs : c) {
            for (int i = 0; i < chs.length; i++) {
                if (a == chs[i]) return true;
            }
        }
        return false;
    }
}
