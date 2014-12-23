package de.mknblch.nolisp.features.list;

import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.inspection.ContainerCloneRule;
import de.mknblch.nolisp.inspection.Inspector;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.codegen.Define;

/**
 * @author mknblch
 */
@Define({"backquote"})
public class BackQuoteSpecialForm extends BuiltInSpecialForm  {

    @Override
    public Object eval(final Interpreter interpreter, final Context context, ListStruct args) throws Exception {
        final Object car = args.car();
        if (!TypeHelper.isList(car)) {
            return car;
        }

        final ContainerCloneRule rule = new ContainerCloneRule() {

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