package de.mknblch.sucode.lexer;

/**
 * basic lisp lexer
 */
public class Lexer {

    public static final char[] IGNORE_CHARS = new char[]{' ', '\t', '\r'};
    public static final char[] NEWLINE_CHARS = new char[]{'\n'};
    public static final char[] SPECIAL_TOKEN_CHARS = new char[]{'(', ')', '\'', '#'};
    public static final char[] DOUBLEQUOTE_CHARS = new char[]{'"'};
    public static final String INT_REGEX = "^\\-?[0-9]+$";
    public static final String REAL_REGEX = "^\\-?[0-9]+\\.[0-9]+$";
    public static final String NIL_REGEX = "^(nil)|(NIL)$";
    public static final String TRUE_REGEX = "^(t)|(T)$";

    public final String code;
    private int offset = 0;

    public Lexer(String code) {
        if (null == code) {
            throw new IllegalArgumentException("null not allowed");
        } else {
            this.code = code.trim();
        }
    }

    public int getOffset() {
        return offset;
    }

    /**
     * reset state
     */
    public void reset() {
        offset = 0;
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
        // return null if end already reached
        if (offset >= code.length()) {
            return null;
        }
        // fetch char at offset and tokenize
        return tokenize(offset, code.charAt(offset));
    }

    private Token tokenize(int position, char c) throws LexerException {
        // first char of each token must decide it's type or it's treated as symbol
        switch (c) {
            case '(':
                offset++;
                return TokenHelper.makeListBeginToken(position);
            case ')':
                offset++;
                return TokenHelper.makeListEndToken(position);
            case '\'':
                offset++;
                return TokenHelper.makeQuoteToken(position);
            case '#':
                offset++;
                return TokenHelper.makeSharpToken(position);
            case ';':
                return TokenHelper.makeCommentToken(tokenizeComment(), position);
            case '"':
                return TokenHelper.makeStringToken(tokenizeString(), position);
            default:
                return decideSymbolType(tokenizeSymbol(), position);
        }
    }

    private Token decideSymbolType(String literal, int position) throws LexerException {

        if (literal.matches(INT_REGEX)) {
            return TokenHelper.makeIntToken(literal, position);
        } else if (literal.matches(REAL_REGEX)) {
            return TokenHelper.makeRealToken(literal, position);
        } else if (literal.matches(NIL_REGEX)) {
            return TokenHelper.makeNilToken(position);
        } else if (literal.matches(TRUE_REGEX)) {
            return TokenHelper.makeTrueToken(position);
        } else {
            return TokenHelper.makeSymbolToken(literal, position);
        }
    }


    private String tokenizeComment() {
        int startIndex = offset;
        until(NEWLINE_CHARS);
        return code.substring(startIndex, offset);
    }

    private String tokenizeSymbol() {
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
