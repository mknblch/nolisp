package de.mknblch.nolisp.core.interpreter.parser.lexer;

import de.mknblch.nolisp.core.interpreter.parser.lexer.constRules.*;
import de.mknblch.nolisp.core.interpreter.parser.lexer.specialTokenRules.*;

import java.util.ArrayList;

/**
 * basic lisp lexer.
 */
public class Lexer extends StringCutter {


    // TODO refactor
    private final char[] tokenDelimiter = new char[]{
            '(', ')', '{', '}', '[', ']',
            ' ', '\t', '\r', '\n'
    };

    private final TokenRule ignorableRule = new IgnorableRule();
    private final AvoidanceRule avoidanceRule = new AvoidanceRule();

    private final ConstRule[] constRules = new ConstRule[] {
        new NullRule(),
        new IntRule(),
        new RealRule(),
        new BigIntegerRule(),
        new BigDecimalRule(),
    };

    private final TokenRule[] tokenRules = new TokenRule[] {
        new ArrayRule(),
        new CommentRule(),
        new ListRule(),
        new StringRule(),
        new SyntacticSugarRule(),
        new QuoteRule(),
    };

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
        // skip all ignorable
        ignorableRule.token(this);
        // check if end reached
        if (!hasNext()) return null;
        // we still have tokens, sync!
        sync();
        // check if any tokenRule matches
        for (TokenRule tokenRule : tokenRules) {
            final Token token = tokenRule.token(this);
            if (null != token) return token;
        }
        // no tokenRule applies, must be a const
        final String literal = tokenizeConst();
        // check if any constRules matches
        for (ConstRule constRule : constRules) {
            final Token token = constRule.token(literal);
            if (null != token) return token;
        }
        // no constRule matches, use avoidanceRule
        return avoidanceRule.token(literal);
    }

    private String tokenizeConst() {
        until(tokenDelimiter);
        return getLiteral();
    }

}
