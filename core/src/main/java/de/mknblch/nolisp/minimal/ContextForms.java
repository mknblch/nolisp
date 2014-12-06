package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.scanner.Define;
import de.mknblch.nolisp.scanner.Special;

import static de.mknblch.nolisp.common.TypeHelper.asList;
import static de.mknblch.nolisp.common.TypeHelper.getSymbolLiteral;

/**
 * @author mknblch
 */
public class ContextForms {

    /*
        bind Vars to global context
     */
    @Special
    @Define({"global", "setq"})
    public static Object setq(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            Expectations.expectCdr(temp);
            final String key = getSymbolLiteral(temp.car());
            value = interpreter.eval(temp.cdr().car(), context);
            context.bindGlobal(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }

    /*
        Bind Vars to local context
     */
    @Special
    @Define({"local"})
    public static Object define(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            final String key = getSymbolLiteral(temp.car());
            Expectations.expectCdr(temp);
            value = interpreter.eval(temp.cdr().car(), context);
            context.bind(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }

    /*
        Bind Vars to the context where they are defined or throw Exception if unknown
     */
    @Special
    @Define({"set"})
    public static Object set(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            final String key = getSymbolLiteral(temp.car());
            Expectations.expectCdr(temp);
            value = interpreter.eval(temp.cdr().car(), context);
            context.bindToContainer(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }


    @Special
    @Define("let*") // (let* ((a 1) (b a)) b) => 1
    public static Object letAsterisk(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
        final ListStruct definitions = asList(args.car());
        for (Object def : definitions) {
            // each element must be a key-value pair (2 element list).
            final ListStruct pair = asList(def);
            // lst must have a successor
            Expectations.expectCdr(pair);
            // bind to local and eval with local scope
            localScope.bind(getSymbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), localScope));
        }
        // evaluate cdar with newly build variable scope
        return interpreter.eval(args.cdr().car(), localScope);
    }

    @Special
    @Define("let") // (let ((a 1) (b a)) b) => ERROR
    public static Object let(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
        // car must be list
        final ListStruct definitions = asList(args.car());
        for (Object def : definitions) {
            // each element must be a value-value pair.
            final ListStruct pair = asList(def);
            Expectations.expectCdr(pair);
            // bind to local but eval args with parent scope
            localScope.bind(getSymbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), parentScope));
        }
        return interpreter.eval(args.cdr().car(), localScope);
    }
}
