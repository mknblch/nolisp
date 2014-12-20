package de.mknblch.nolisp.parser.lexer;

import de.mknblch.nolisp.parser.lexer.rules.*;
import de.mknblch.nolisp.parser.lexer.rules.AvoidanceRule;
import de.mknblch.nolisp.parser.lexer.rules.IgnoreRule;

/**
 * minimal lisp lexer.
 */
public class Lexer extends StringCutter {


    // TODO refactor
    private final char[] delimiter = new char[]{
            '(', ')', '{', '}', '[', ']',
            ' ', '\t', '\r', '\n'
    };

    private final TokenRule ignoreRule = new IgnoreRule();
    private final ConstRule avoidanceRule = new AvoidanceRule();

    private final ConstRule[] constRules = new ConstRule[] {
        new NullRule(),
        new IntRule(),
        new LongRule(),
        new RealRule(),
        new HexadecimalRule(),
        new BigIntegerRule(),
        new BigDecimalRule(),
    };

    private final TokenRule[] tokenRules = new TokenRule[] {
        new ArrayRule(),
        new CommentRule(),
        new ListRule(),
        new StringRule(),
        new SyntacticSugarRule(),
    };

    public void setCode(String code) {
        if (null == code) {
            throw new IllegalArgumentException("null not allowed");
        } else {
            setString(code.trim());
        }
    }

    /**
     * fetch next token.
     *
     * phase 1: uses a special rule to skip unwanted chars
     *  (e.g. whitespaces, newlines).<br/>
     *
     * phase 2: checks if any token rule matches and return its token. these
     *  rules must decide if they match by checking the current head of the
     *  StringCutter. <br/>
     *
     * phase 3: is started when there are characters left but all
     *  ignorable chars have been skipped and no tokenRule matches.
     *  it must be a const type. check if any constRule matches.<br/>
     *
     * phase 4: use the avoidanceRule which must clearly decide its type and return a
     *  standard token.<br/>
     */
    public Token next() throws LexerException {
        // skip all ignorable
        ignoreRule.token(this);
        // check if end reached
        if (!hasNext()) return null;
        // we still have tokens and we must be at the start of something special, do sync
        sync();
        // check if a tokenRule matches
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
        until(delimiter);
        return getLiteral();
    }

}
