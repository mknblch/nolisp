package de.mknblch.nolisp.parser.lexer;

/**
 * @author mknblch
 */
public interface ConstType {

    public boolean matches(String literal);

    public Token tokenize(String literal);
}
