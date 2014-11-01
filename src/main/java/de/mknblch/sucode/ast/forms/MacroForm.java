package de.mknblch.sucode.ast.forms;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.Interpreter;

import java.util.ArrayList;
import java.util.List;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * (defmacro target (arg*) from+)
 *
 * @author mknblch
 */
public class MacroForm extends SpecialForm {

    private final String symbol;
    private final ListStruct forms;
    private final List<ListStruct>[] index;
    private final List<String> formSymbols;

    // (defmacro symbol (args*) from+)
    public MacroForm(String symbol, List<String> formSymbols, ListStruct forms) {
        index = buildIndex(formSymbols, forms);
        this.symbol = symbol;
        this.formSymbols = formSymbols;
        this.forms = forms;
    }

    @Override // args=(arg1 arg2 ...)
    public Object eval(Interpreter interpreter, Context localContext, ListStruct args) throws Exception {
        final List<Object> flatten = asJavaList(args);
        replace(index, flatten, forms);

        System.out.printf("macroeval: %s%n", FormatHelper.formatPretty(forms));
        Object ret = null;
        for (Object o : forms) {
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

    public Object getForms() {
        return forms;
    }

    private static List<ListStruct>[] buildIndex(List<String> formSymbols, ListStruct form) {
        final List<ListStruct>[] index = new List[formSymbols.size()];
        extractPointer(index, formSymbols, form);
        return index;
    }

    private static void extractPointer(List<ListStruct>[] list, List<String> formSymbols, ListStruct form) {
        // find each occurrence of the formSymbols and bind it's parent ListStruct to the map
        ListStruct temp = form;
        while (null != temp) {
            final Object car = temp.car();
            if (isList(car)) {
                extractPointer(list, formSymbols, (ListStruct) car);
            } else {
                final int matchingSymbol = findMatchingSymbol(formSymbols, car);
                if (matchingSymbol != -1) {
                    if (null == list[matchingSymbol]) list[matchingSymbol] = new ArrayList<ListStruct>();
                    list[matchingSymbol].add(temp);
                }
            }
            temp = temp.cdr();
        }
    }

    private static int findMatchingSymbol(List<String> search, Object o) {
        if (!(o instanceof SymbolStruct)) return -1;
        final String literal = ((SymbolStruct) o).literal;
        for (int i = search.size()-1; i >= 0; i--) {
            if(search.get(i).equals(literal)) return i;
        }
        return -1;
    }

    private static void replace(List<ListStruct>[] index, List<Object> replacements, Object form) {
        final int min = Math.min(index.length, replacements.size());
        for (int i = 0; i < min; i++) {
            final List<ListStruct> listStructs = index[i];
            final Object o = replacements.get(i);
            if (null == o) continue;
            for (ListStruct listStruct : listStructs) {
                listStruct.setCar(o);
            }
        }
    }

}
