package de.mknblch.nolisp.parser.lexer;

/**
 * @author mknblch
 */
public class TokenHelper {

    public static Token makeStringToken(String literal) throws LexerException {

        if (literal.length() < 2) {
            throw new LexerException(String.format("Parsing '%s' to STRING failed", literal));
        }
        return new Token(Token.Type.CONST, literal, literal.substring(1, literal.length() - 1));
    }

    public static Token makeIntToken(String literal) throws LexerException {
        try {
            return new Token(Token.Type.CONST, literal, Integer.parseInt(literal));
        } catch (Exception e) {
            throw new LexerException(String.format("[@%04d] Parsing '%s' to INT failed", literal), e);
        }
    }

    public static Token makeRealToken(String literal) throws LexerException {
        try {
            return new Token(Token.Type.CONST, literal, Double.parseDouble(literal));
        } catch (Exception e) {
            throw new LexerException(String.format("Parsing '%s' to REAL failed", literal), e);
        }
    }

    public static Token makeTrueToken() {
        return new Token(Token.Type.CONST, "true", Boolean.TRUE);
    }

    public static Token makeFalseToken() {
        return new Token(Token.Type.CONST, "false", Boolean.FALSE);
    }

    public static Token makeNilToken() {
        return new Token(Token.Type.CONST, "nil", null);
    }

    public static Token makeSymbolToken(String literal) {
        return new Token(Token.Type.SYMBOL, literal, literal);
    }

    public static Token makeCommentToken(String literal) {
        return new Token(Token.Type.LINE_COMMENT, literal, literal);
    }

    public static Token makeListBeginToken() {
        return new Token(Token.Type.LIST_BEGIN, "(", "(");
    }

    public static Token makeListEndToken() {
        return new Token(Token.Type.LIST_END, ")", ")");
    }

    public static Token makeQuoteToken() {
        return new Token(Token.Type.QUOTE, "'", "'");
    }

    public static Token makeSharpToken() {
        return new Token(Token.Type.SHARP, "#", "#");
    }

    public static Token makeBackquoteToken() {
        return new Token(Token.Type.BACKQUOTE, "`", "`");
    }

    public static Token makeCommaToken() {
        return new Token(Token.Type.COMMA, ",", ",");
    }

    public static Token makeSpliceToken() {
        return new Token(Token.Type.SPLICE, "@", "@");
    }
}
