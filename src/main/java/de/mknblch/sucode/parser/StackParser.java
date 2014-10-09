package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Token;
import de.mknblch.sucode.parser.structs.*;

import java.util.List;
import java.util.Stack;

/**
 * Created by mknblch on 05.10.2014.
 */

public class StackParser {

    public ListStruct parse (List<Token> tokenList) throws ParserException {

        final ListStruct root = new ListStruct();
        final Stack<ListStruct> stack = new Stack<ListStruct>();
        boolean quoted = false;

        stack.push(root);

        for (int i = 0; i < tokenList.size(); i++) {

            final Token token = tokenList.get(i);
            switch (token.type) {
                case BRACE_OPEN: {
                    final ListStruct item = quoted ? new QuotedListStruct() : new ListStruct();
                    if (stack.isEmpty()) {
                        throw new ParserException("Unbalanced AST");
                    }
                    stack.peek().addCons(item);
                    stack.push(item);
                    quoted = false;
                    break;
                }
                case BRACE_CLOSE:
                    final ListStruct pop = stack.pop();
                    if (null == pop) {
                        throw new ParserException("Unbalanced AST");
                    }
                    break;
                case SYMBOL:
                    stack.peek().addCons(joinQuoted(new SymbolStruct(token.literal), quoted));
                    quoted = false;
                    break;
                case STRING:
                    stack.peek().addCons(joinQuoted(new StringStruct(token.literal), quoted));
                    quoted = false;
                    break;
                case INT:
                    stack.peek().addCons(joinQuoted(new IntStruct(token.literal), quoted));
                    quoted = false;
                    break;
                case REAL:
                    stack.peek().addCons(joinQuoted(new RealStruct(token.literal), quoted));
                    quoted = false;
                    break;
                case QUOTE:
                    quoted = true;
                    break;
                case LINE_COMMENT: // do nothing
                    break;
            }
        }


        if (stack.isEmpty() || stack.peek() != root){

            while (!stack.isEmpty()) {
                ListStruct pop = stack.pop();
                System.out.println(pop.car());
            }

            throw new ParserException("Unbalanced AST");
        }

        return root;
    }

    private static Atom joinQuoted(Atom atom, boolean quoted) {

        if (quoted) {
            return new QuotedListStruct(atom);
        } else {
            return atom;
        }
    }

}
