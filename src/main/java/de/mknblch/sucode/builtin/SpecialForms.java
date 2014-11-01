package de.mknblch.sucode.builtin;

import de.mknblch.sucode.ast.ListStruct;
import de.mknblch.sucode.ast.SymbolStruct;
import de.mknblch.sucode.ast.forms.Form;
import de.mknblch.sucode.ast.forms.LambdaForm;
import de.mknblch.sucode.ast.forms.MacroForm;
import de.mknblch.sucode.func.Define;
import de.mknblch.sucode.func.Special;
import de.mknblch.sucode.helper.FormatHelper;
import de.mknblch.sucode.inspection.Inspector;
import de.mknblch.sucode.inspection.Rule;
import de.mknblch.sucode.inspection.RuleAdapter;
import de.mknblch.sucode.interpreter.Context;
import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.parser.Parser;

import java.util.List;

import static de.mknblch.sucode.helper.Expectations.expectCdr;
import static de.mknblch.sucode.helper.Expectations.expectList;
import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class SpecialForms {

    @Special
    @Define("setq")
    public static Object setq(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        ListStruct temp = args;
        Object value;
        do {
            final String key = symbolLiteral(temp.car());
            expectCdr(temp);
            value = interpreter.eval(temp.cdr().car(), context);
            context.bindGlobal(key, value);
            temp = temp.cdr().cdr();
        } while (temp != null);

        return value;
    }

    @Special
    @Define("quote")
    public static Object quote(Interpreter interpreter, Context context, ListStruct args) throws EvaluationException {
        return args.car();
    }

    @Define("list")
    public static Object list(Context context, ListStruct args) throws EvaluationException {
        return args;
    }

    @Special
    @Define("let*") // (let* ((a 1) (b a)) b) => 1
    public static Object letAsterisk(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
        // car must be list
        for (Object def : (ListStruct) args.car()) {
            // each element must be a value-value pair.
            final ListStruct pair = ((ListStruct) def);
            expectCdr(pair);
            // bind to local and eval with local scope
            localScope.bind(symbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), localScope));
        }
        // evaluate cdar with newly build variable scope
        return interpreter.eval(args.cdr().car(), localScope);
    }

    @Special
    @Define("let") // (let ((a 1) (b a)) b) => ERROR
    public static Object let(Interpreter interpreter, Context parentScope, ListStruct args) throws Exception {
        final Context localScope = parentScope.derive();
        // car must be list
        final ListStruct car = (ListStruct) args.car();
        expectList(car);
        for (Object def : car) {
            // each element must be a value-value pair.
            final ListStruct pair = ((ListStruct) def);
            expectCdr(pair);
            // bind to local but eval args with parent scope
            localScope.bind(symbolLiteral(pair.car()), interpreter.eval(pair.cdr().car(), parentScope));
        }
        return interpreter.eval(args.cdr().car(), localScope);
    }

    @Define("progn") // (progn 1 2 3) => 3
    public static Object progn(Context parentScope, ListStruct args) throws Exception {
        if (null == args) {
            return null;
        }
        return args.last().car();
    }

    @Special
    @Define("lambda") // ((lambda (a) (+ a 1)) 1) => 2
    public static Object lambda(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        expectCdr(args);
        return new LambdaForm(interpreter, parentContext, symbolList(args.car()), args.cdar());
    }

    @Special
    @Define("defun") // (defun bla (a) (+ a 1) ) => form
    public static Object defun(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        // TODO review
        expectCdr(args);
        final String functionName = symbolLiteral(args.car());
        final List<String> symbols = symbolList(args.cdar());
        final LambdaForm lambda = new LambdaForm(interpreter, parentContext, symbols, args.cddar());
        parentContext.bindGlobal(functionName, lambda);
        return lambda;
    }

    @Special
    @Define("eval") // (eval '(+ 20 22)) :> (eval (quote (+ 20 22))) => 42
    public static Object eval(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        // TODO Test
        return interpreter.eval(interpreter.eval(args.car(), parentContext), parentContext);
    }

    @Special
    @Define("function") // (function +)
    public static Object function(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
//       TODO REVIEW & TEST
        final Object eval = interpreter.eval(args.car(), parentContext);
        return interpreter.eval(eval, parentContext);
    }

    @Special
    @Define("funcall") // (funcall (function +) 1 2 3 4 5) => 15
    public static Object funcall(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        final Object car = args.car();
        final Form form = asForm(interpreter.eval(car, parentContext));
        Object ret = args.cdar();
        ListStruct rest = args.cddr();
        for (Object o : rest) {
            ret = form.eval(parentContext, new ListStruct(ret, interpreter.eval(o, parentContext)));
        }
        return ret;
    }

    @Special
    @Define("defmacro") // (defmacro name (arg*) form+)
    public static Object defmacro(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {
        expectCdr(args);
        final Object symbol = args.car();
        final String name = symbolLiteral(symbol);


        final ListStruct forms = args.cddr();

        System.out.printf("forms: %s%n", FormatHelper.formatPretty(forms));

        final MacroForm macroForm = new MacroForm(name, symbolList(args.cdar()), forms);



        parentContext.bind(name, macroForm);
        return symbol;
    }

    @Special
    @Define("backquote")
    public static Object backquote(final Interpreter interpreter, final Context parentContext, ListStruct args) throws Exception {

        final Object car = args.car();
        if (!isList(car)) {
            return car;
        }

        final Rule replacementRule = new RuleAdapter() {
            @Override
            public void inspect(ListStruct container, Object element) throws Exception {
                // evaluate (comma <form>) structs only
                if (isList(element) && ((ListStruct) element).car() == Parser.COMMA_STRUCT) {
                    container.setCar(interpreter.eval(((ListStruct) element).cdar(), parentContext));
                }
            }

            @Override
            public boolean inspectSublists() {
                return true;
            }

            @Override
            public boolean follow(ListStruct container, ListStruct listElement) {
                return true;
            }
        };
        Inspector.inspect(args, replacementRule);
        return args.car(); //
    }

    @Special
    @Define("comma")
    public static Object comma(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {

        throw new EvaluationException("Misplaced COMMA");
    }

    @Special
    @Define("splice")
    public static Object splice(Interpreter interpreter, Context parentContext, ListStruct args) throws Exception {

        final Object car = args.car();


        return null;
    }
}
