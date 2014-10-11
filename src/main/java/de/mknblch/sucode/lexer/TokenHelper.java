package de.mknblch.sucode.lexer;

/**
 * Created by mknblch on 10.10.2014.
 */
public class TokenHelper {

    public static Token makeStringToken(String literal, int position) throws LexerException {

        if (literal.length() < 2) {
            throw new LexerException(String.format("[@%04d] Parsing '%s' to STRING failed", position, literal));
        }
        return new Token(Token.Type.STRING, literal, literal.substring(1, literal.length() - 1), position);
    }

    public static Token makeIntToken(String literal, int position) throws LexerException {
        try {
            return new Token(Token.Type.INT, literal, Integer.parseInt(literal), position);
        } catch (Exception e) {
            throw new LexerException(String.format("[@%04d] Parsing '%s' to INT failed", position, literal));
        }
    }

    public static Token makeRealToken(String literal, int position) throws LexerException {
        try {
            return new Token(Token.Type.REAL, literal, Double.parseDouble(literal), position);
        } catch (Exception e) {
            throw new LexerException(String.format("[@%04d] Parsing '%s' to REAL failed", position, literal));
        }
    }

    public static Token makeTrueToken(int position) {
        return new Token(Token.Type.TRUE, "t", Boolean.TRUE, position);
    }

    public static Token makeNilToken(int position) {
        return new Token(Token.Type.NIL, "nil", null, position);
    }

    public static Token makeSymbolToken(String literal, int position) {
        return new Token(Token.Type.SYMBOL, literal, literal, position);
    }

    public static Token makeCommentToken(String literal, int position) {
        return new Token(Token.Type.LINE_COMMENT, literal, literal, position);
    }

    public static Token makeListBeginToken(int position) {
        return new Token(Token.Type.LIST_BEGIN, "(", "(", position);
    }

    public static Token makeListEndToken(int position) {
        return new Token(Token.Type.LIST_END, ")", ")", position);
    }

    public static Token makeQuoteToken(int position) {
        return new Token(Token.Type.QUOTE, "'", "'", position);
    }

    public static Token makeSharpToken(int position) {
        return new Token(Token.Type.SHARP, "#", "#", position);
    }
}
