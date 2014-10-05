package de.mknblch.sucode.parser;

import de.mknblch.sucode.lexer.Token;
import de.mknblch.sucode.parser.structs.*;

import java.util.List;
import java.util.Stack;

/**
 * Created by pexx on 05.10.2014.
 */
public class Parser {


    public static final SymbolStruct QUOTE_STRUCT = new SymbolStruct("quote");

    public ListStruct parse (List<Token> tokenList) throws ParserException {

        ListStruct root = new ListStruct();

        final Stack<ListStruct> stack = new Stack<ListStruct>();
        stack.push(root);

        for (int i = 0; i < tokenList.size(); i++) {

            final Token token = tokenList.get(i);

            switch (token.type) {

                case SYMBOL:
                    stack.peek().add(new SymbolStruct(token.literal));
                    break;
                case BRACE_OPEN: {
                    ListStruct item = new ListStruct();
                    stack.peek().add(item);
                    stack.push(item);
                    break;
                }
                case BRACE_CLOSE:
                    ListStruct pop = stack.pop();
                    if (null == pop) {
                        throw new ParserException(String.format("Unbalanced AST at Token %02d (@ %04d)", i, token.position));
                    }
                    break;
                case STRING:
                    stack.peek().add(new StringStruct(token.literal));
                    break;
                case INT:
                    stack.peek().add(new IntStruct(token.literal));
                    break;
                case REAL:
                    stack.peek().add(new RealStruct(token.literal));
                    break;
                case QUOTE: {
                    final ListStruct item = new ListStruct();
                    stack.peek().add(item);
                    stack.push(item);
                    item.add(QUOTE_STRUCT);
                    break;
                }
                case LINE_COMMENT: // do nothing
                    break;
            }
        }

        return root;
    }

}
