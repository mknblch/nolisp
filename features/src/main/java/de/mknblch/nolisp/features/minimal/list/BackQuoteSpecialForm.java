package de.mknblch.nolisp.features.minimal.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.SpecialForm;
import de.mknblch.nolisp.generator.annotations.Define;
import de.mknblch.nolisp.inspection.CloneRule;
import de.mknblch.nolisp.inspection.Inspector;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;

/**
 * @author mknblch
 */
@Define({"backquote"})
public class BackQuoteSpecialForm implements SpecialForm {

    @Override
    public Object eval(final Interpreter interpreter, final Context context, ListStruct args) throws Exception {
        final Object car = args.car();
        if (!TypeHelper.isList(car)) {
            return car;
        }

        final CloneRule rule = new CloneRule() {

            @Override
            public ListStruct cloneSublist(ListStruct container) throws Exception {

                if (TypeHelper.isSymbolWithLiteral(container.car(), "comma-splice")) {
                    final ListStruct listStruct = TypeHelper.asList(interpreter.eval(container.cadr(), context));
                    final ListStruct clone = new ListStruct();
                    //noinspection ConstantConditions
                    for (Object o : listStruct) {
                        clone.add(o);
                    }
                    return clone;
                }

                else if (TypeHelper.isSymbolWithLiteral(container.car(), "comma")) {
                    return new ListStruct(interpreter.eval(container.cadr(), context));
                }

                return new ListStruct(container);
            }

            @Override
            public ListStruct cloneElement(Object element) throws Exception {
                return new ListStruct(element);
            }
        };

        return Inspector.cloneTree(args, rule).car();
    }
}