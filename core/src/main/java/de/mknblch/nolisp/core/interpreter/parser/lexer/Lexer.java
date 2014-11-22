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
    private final ArrayList<ConstDecisionRule> decisionRules = new ArrayList<ConstDecisionRule>() {{
        add(new NullRule());
        add(new IntRule());
        add(new RealRule());
        add(new BigIntegerRule());
        add(new BigDecimalRule());
    }};

    private final ArrayList<TokenDecisionRule> tokenDecisionRules = new ArrayList<TokenDecisionRule>() {{
        add(new ArrayRule());
        add(new CommentRule());
        add(new ListRule());
        add(new StringRule());
        add(new SpecialTokenRule());
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

        for (TokenDecisionRule tokenDecisionRule : tokenDecisionRules) {
            final Token token = tokenDecisionRule.decide(c, this);
            if (null != token) return token;
        }

        final String literal = tokenizeConst();
        for (ConstDecisionRule decisionRule : decisionRules) {
            final Token token = decisionRule.token(literal);
            if (null != token) return token;
        }
        return new Token(Token.Type.SYMBOL, literal, literal);
    }

    /**
     * skip ignorable chars. e.g. whitespaces and newline
     */
    private void skipIgnorable() {
        skip(IGNORE_CHARS, NEWLINE_CHARS);
    }

    private String tokenizeConst() {
        until(IGNORE_CHARS, SPECIAL_TOKEN_CHARS, NEWLINE_CHARS);
        return getLiteral();
    }

}
