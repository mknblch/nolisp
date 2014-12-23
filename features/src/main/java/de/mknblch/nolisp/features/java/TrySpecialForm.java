package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SymbolStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.generator.Define;

import static de.mknblch.nolisp.common.TypeHelper.asList;
import static de.mknblch.nolisp.common.TypeHelper.asSymbol;
import static de.mknblch.nolisp.common.TypeHelper.getSymbolLiteral;

/**
 * @author mknblch
 */
@Define({"try"})
public class TrySpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object tryBlock = args.car();
        final ListStruct catchBlocks = TypeHelper.asList(args.cadr());
        try {
            return interpreter.eval(tryBlock, context);
        } catch (Exception e) {
            final Class<?> exClazz = e.getClass();
            for (Object element : catchBlocks) {
                final ListStruct listStruct = TypeHelper.asList(element);
                Expectations.expectSymbolWithLiteral(listStruct.car(), "catch");
                final SymbolStruct exClassSymbol = TypeHelper.asSymbol(listStruct.cadr());
                if(Class.forName(exClassSymbol.literal).isAssignableFrom(exClazz)) {
                    final String literal = TypeHelper.getSymbolLiteral(listStruct.caddr());
                    final Context derive = context.derive();
                    derive.bind(literal, e);
                    return interpreter.eval(listStruct.nth(3), derive);
                }
            }
            throw e;
        }
    }
}