package de.mknblch.sucode.interpreter.forms;

import de.mknblch.sucode.interpreter.EvaluationException;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by mknblch on 11.10.2014.
 */
public class FormRegister {

    private final Map<String, Form> forms = new HashMap<String, Form>();

    public void addForm(Form form) {
        forms.put(form.getSymbol(), form);
    }

    public void removeForm(Form form) {
        forms.remove(form.getSymbol());
    }

    public void clear() {
        forms.clear();
    }

    public Form getForm(String symbol) throws EvaluationException {
        final Form form = forms.get(symbol);
        if (null != form) {
            return form;
        }
        throw new EvaluationException(String.format("Function '%s' not found.", symbol));
    }
}
