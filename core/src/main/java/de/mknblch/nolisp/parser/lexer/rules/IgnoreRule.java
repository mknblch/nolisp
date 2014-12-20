package de.mknblch.nolisp.parser.lexer.rules;

import de.mknblch.nolisp.parser.lexer.StringCutter;
import de.mknblch.nolisp.parser.lexer.Token;
import de.mknblch.nolisp.parser.lexer.TokenRule;

/**
 * special rule used for skipping ignorable chars.
 *
 * @author mknblch
 */
public class IgnoreRule implements TokenRule {

    private static final char[] IGNORE_CHARS = new char[]{' ', '\t', '\r', '\n'};

    @Override
    public Token token(StringCutter cutter) {
        cutter.skip(IGNORE_CHARS);
        return null;
    }
}
