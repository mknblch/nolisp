package de.mknblch.nolisp.features.macro;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.annotations.Define;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"defmacro"})
public class DefMacroSpecialForm implements SpecialForm {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        Expectations.expectCdr(args);
        final Object symbol = args.car();
        context.bind(TypeHelper.getSymbolLiteral(symbol), new Macro(TypeHelper.convertToSymbolList(args.cadr()), args.cddr()));
        return symbol;
    }
}