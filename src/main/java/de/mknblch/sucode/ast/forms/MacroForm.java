package de.mknblch.sucode.ast.forms;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

import java.util.List;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 *
 *
 * (defmacro target (arg*) from+)
 *
 * @author mknblch
 */
public class MacroForm extends SpecialForm {

    private final String symbol;
    private final List<String> formSymbols;
    private final ListStruct form;

    // (defmacro symbol (args*) from+)
    public MacroForm(String symbol, List<String> formSymbols, ListStruct form) {
        this.symbol = symbol;
        this.formSymbols = formSymbols;
        this.form = form;
    }

    @Override // args=(arg1 arg2 ...)
    public Object eval(Interpreter interpreter, Context localContext, ListStruct args) throws Exception {
        final List<Object> flatten = asJavaList(args);
        final ListStruct formList = map(formSymbols, flatten, form);
        Object ret = null;
        for (Object o : formList) {
            ret = interpreter.eval(o, localContext);
        }
        return ret;
    }


    @Override
    public String getSymbol() {
        return symbol;
    }

    @Override
    public Type getType() {
        return Type.MACRO;
    }

    public List<String> getFormSymbols() {
        return formSymbols;
    }

    public Object getForm() {
        return form;
    }

    /**
     * performs a deep copy of form and replace each occurrence of search with replacement.
     */
    private static ListStruct map(List<String> searchSymbols, List<Object> replacements, ListStruct form) {
        final int rSize = replacements.size();
        final ListStruct node = new ListStruct();
        for (Object o : form) {
            if(isList(o)) {
                // recursive call to go down into sub-list
                node.add(map(searchSymbols, replacements, (ListStruct) o));
            } else {
                final int matchingSymbol = findMatchingSymbol(searchSymbols, o);
                if (matchingSymbol != -1) {
                    // search was found, replace
                    // has replacement for index
                    if(rSize > matchingSymbol) node.add(replacements.get(matchingSymbol));
                    // has no replacement, ass null
                    // TODO review
                    else node.add(null);
                } else {
                    // add as is
                    node.add(o);
                }
            }
        }

        return node;
    }

    private static int findMatchingSymbol(List<String> search, Object o) {
        if (!(o instanceof SymbolStruct)) return -1;
        final String literal = ((SymbolStruct) o).literal;
        for (int i = search.size()-1; i >= 0; i--) {
            if(search.get(i).equals(literal)) return i;
        }
        return -1;
    }

}
