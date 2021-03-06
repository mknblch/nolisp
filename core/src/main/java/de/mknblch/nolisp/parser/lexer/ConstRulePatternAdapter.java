package de.mknblch.nolisp.parser.lexer;

import de.mknblch.nolisp.parser.lexer.ConstRule;
import de.mknblch.nolisp.parser.lexer.LexerException;
import de.mknblch.nolisp.parser.lexer.Token;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author mknblch
 */
public abstract class ConstRulePatternAdapter implements ConstRule {

    private final Pattern pattern;

    public ConstRulePatternAdapter(String regex) {
        this.pattern = Pattern.compile(regex);
    }

    @Override
    public Token token(String literal) throws LexerException {
        final Matcher matcher = pattern.matcher(literal);
        if(!matcher.matches()) return null;
        return construct(literal, matcher);
    }

    protected abstract Token construct (String literal, Matcher matcher) ;
}
