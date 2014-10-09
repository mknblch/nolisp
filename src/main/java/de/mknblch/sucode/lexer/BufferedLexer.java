package de.mknblch.sucode.lexer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * maybe useful later
 *
 * @author mknblch
 * @date 09.10.2014.
 */
public class BufferedLexer {

    private final List<Token> tokens;
    private int index = 0;

    public BufferedLexer(Lexer lexer) throws LexerException {
        tokens = asList(lexer);
    }

    public boolean hasNext() {
        return index < tokens.size();
    }

    public boolean hasPrevious() {
        return index > 0;
    }

    public Token next() {
        return tokens.get(index++);
    }

    public Token previous() {
        return tokens.get(--index);
    }

    public void reset() {
        index = 0;
    }

    private static List<Token> asList(Lexer lexer) throws LexerException {
        final ArrayList<Token> codeList = new ArrayList<Token>();
        while (lexer.hasNext()) {
            codeList.add(lexer.next());
        }
        return Collections.unmodifiableList(codeList);
    }
}
