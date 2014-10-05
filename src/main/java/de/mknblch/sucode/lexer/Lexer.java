package de.mknblch.sucode.lexer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * basic lisp lexer
 */
public class Lexer {

    public static final char[] IGNORE = new char[]{' ', '\t', '\r'};
    public static final char[] NEWLINE = new char[]{'\n'};
    public static final char[] SPECIAL_TOKEN = new char[]{'(', ')', '\''};
    public static final char[] DOUBLEQUOTE = new char[]{'"'};

    public final String code;
    private int offset = 0;

    public Lexer(String code) {
        if (null == code) {
            throw new IllegalArgumentException("null not allowed");
        } else {
            this.code = new String(code.trim());
        }
    }

    /**
     * reset state
     */
    public void reset() {
        offset = 0;
    }

    /**
     * determine if lexer has more tokens.
     * @return true if tokens left
     */
    public boolean hasNext() {
        return offset < code.length();
    }

    public List<Token> asList() throws LexerException {
        final ArrayList<Token> codeList = new ArrayList<Token>();
        while (hasNext()) {
            codeList.add(next());    
        }
        return Collections.unmodifiableList(codeList);
    }

    /**
     * fetch next token and increment offset.
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

        // first char of each token must decide it's type
        switch(c) {
            case '(' :
                offset++;
                return new Token(position, Token.Type.BRACE_OPEN, "(");
            case ')' :
                offset++;
                return new Token(position, Token.Type.BRACE_CLOSE, ")");
            case '\'' :
                offset++;
                return new Token(position, Token.Type.QUOTE, "'");
            case ';' :
                return new Token(position, Token.Type.LINE_COMMENT, tokenizeComment());
            case '"' :
                return new Token(position, Token.Type.STRING, tokenizeString());
            default :
                return decideSymbolType(position, tokenizeSymbol());
        }
    }

    private Token decideSymbolType(int position, String symbol) {
        if(symbol.matches("^\\-?[0-9]+$"))
            return new Token(position, Token.Type.INT, symbol);
        else if(symbol.matches("^\\-?[0-9]+\\.[0-9]+$"))
            return new Token(position, Token.Type.REAL, symbol);
        else
            return new Token(position, Token.Type.SYMBOL, symbol);
    }


    private String tokenizeComment() {
        int startIndex = offset;
        until(NEWLINE);
        return code.substring(startIndex, offset);
    }

    private String tokenizeSymbol() {
        int startIndex = offset;
        until(IGNORE, SPECIAL_TOKEN, NEWLINE);
        return code.substring(startIndex, offset);
    }

    private String tokenizeString() throws LexerException {
        // store start index of string including " and inc offset
        int startIndex = offset++;
        until(DOUBLEQUOTE);
        // inc offset and store end of string including "
        int endIndex = ++offset;
        // if the last increment grows offset above code.length throw an exception
        if(offset > code.length()) {
            throw new LexerException(String.format("[@%04d] premature end of string found", startIndex));
        }
        // TODO escaped "
        return code.substring(startIndex, endIndex);
    }

    /**
     * skip ignorable chars. e.g. whitespaces and newline
     */
    private void skipIgnorable() {
        skip(IGNORE, NEWLINE);
    }

    /**
     * increment offset until any char OTHER THEN charsToSkip is found
     * @param charsToSkip these chars should be ignored
     */
    private void skip(char[]... charsToSkip) {
        for (int i = offset; i < code.length(); i++) {
            if(elementOf(code.charAt(offset), charsToSkip))
                offset++;
            else
                return;
        }
    }

    /**
     * increment offset until any of charsToStop is found
     * @param charsToStop chars to search for
     */
    private void until(char[]... charsToStop) {
        for (int i = offset; i < code.length(); i++) {
            if(!elementOf(code.charAt(offset), charsToStop))
                offset++;
            else
                return;
        }
    }

    /**
     * determine if char a is element of c
     * @param a single char for comparision
     * @param c set of chars
     * @return true if a elementOf c. false otherwise
     */
    public static boolean elementOf(char a, char[]... c) {
        for (char[] chs : c) {
            for (int i = 0; i < chs.length; i++) {
                if (a == chs[i])
                    return true;
            }
        }
        return false;
    }
}
