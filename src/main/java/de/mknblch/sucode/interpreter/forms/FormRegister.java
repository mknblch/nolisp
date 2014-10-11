package de.mknblch.sucode.interpreter.forms;

import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.parser.structs.ListStruct;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Created by mknblch on 11.10.2014.
 */
public class FormRegister {

    private final Map<String, Form> forms = new HashMap<String, Form>();

    public void addForm(Form form) {
        if (null == form.getSymbol() || form.getSymbol().isEmpty()) {
            throw new IllegalArgumentException("Form had no symbol.");
        }
        forms.put(form.getSymbol(), form);
    }

    public void removeForm(String symbol) {
        forms.remove(symbol);
    }

    public boolean containsForm(String symbol) {
        return forms.containsKey(symbol);
    }

    public void clear() {
        forms.clear();
    }

    public Set<String> keySet() {
        return forms.keySet();
    }

    public int size() {
        return forms.size();
    }

    public Form getForm(String symbol) throws EvaluationException {
        final Form form = forms.get(symbol);
        if (null != form) {
            return form;
        }
        throw new EvaluationException(String.format("Function '%s' not found.", symbol));
    }

    public void register(Class<?> clazz) throws FormException {
        // scan class
        final Method[] declaredMethods = clazz.getDeclaredMethods();
        boolean formsFound = false;
        for (final Method method : declaredMethods) {
            // check method signature
            if (!suitable(method)) continue;

            final String[] symbols = method.getAnnotation(Function.class).symbol();

            // register with method 1name as symbol
            if (symbols.length == 0) {
                registerMethod(method, method.getName());
            } else {
                for (String methodName : symbols) {
                    registerMethod(method, methodName);
                }
            }
            formsFound = true;
        }
        if(!formsFound) throw new FormException(String.format("No suitable methods found in Class '%s'.", clazz.getName()));
    }

    private void registerMethod(final Method candidate, final String function) {
        addForm(
            new Form() {
                private final Method method = candidate;
                @Override
                public Object eval(ListStruct args, Environment environment, Interpreter interpreter) throws EvaluationException {
                    try {
                        return method.invoke(null, args, environment, interpreter);
                    } catch (IllegalAccessException e) {
                        throw new EvaluationException(e);
                    } catch (InvocationTargetException e) {
                        throw new EvaluationException(e);
                    }
                }
                @Override
                public String getSymbol() {
                    return function;
                }
            }
        );
    }

    /**
     * checks if the method signature is suitable for Forms.
     */
    private boolean suitable(Method method) throws FormException {
        if(!method.isAnnotationPresent(Function.class)) return false;
        if(method.getReturnType().equals(Void.TYPE)) throw new FormException("Invalid signature. Function must have a return type.");
        if(!Modifier.isStatic(method.getModifiers())) throw new FormException("Invalid signature. Function must be static.");
        final Class<?>[] types = method.getParameterTypes();
        if(
                3 != types.length ||
                !ListStruct.class.equals(types[0]) ||
                !Environment.class.equals(types[1]) ||
                !Interpreter.class.equals(types[2])) throw new FormException("Invalid signature. Function must match 'Object func(ListStruct, Environment, Interpreter)'");

        return true;
    }
}
