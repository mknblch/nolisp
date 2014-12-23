package de.mknblch.nolisp.features.macro;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.generator.Define;

/**
 * @author mknblch
 */
@Define({"defmacro"})
public class DefMacroSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object symbol = args.car();
        context.bind(TypeHelper.getSymbolLiteral(symbol), new Macro(TypeHelper.convertToSymbolList(args.cadr()), args.cddr()));
        return symbol;
    }
}