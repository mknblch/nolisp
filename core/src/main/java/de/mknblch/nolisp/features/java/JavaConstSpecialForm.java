package de.mknblch.nolisp.features.java;

import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.datatypes.builtin.BuiltInSpecialForm;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.scanner.Define;

import java.lang.reflect.Field;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author mknblch
 */
@Define({"java-const", "jconst"})
public class JavaConstSpecialForm extends BuiltInSpecialForm  {

    private static final Pattern CLASS_METHOD_PATTERN = Pattern.compile("(.+):(.+)");

    @Override
    public Object eval(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final String fqName = TypeHelper.getSymbolLiteral(args.car());
        final Matcher matcher = CLASS_METHOD_PATTERN.matcher(fqName);
        if(!matcher.matches()) {
            throw new EvaluationException(
                    String.format("Invalid literal in java constant retrieval, given %s.",
                            FormatHelper.formatPretty(args.car())));
        }
        final Class<?> clazz = Class.forName(matcher.group(1));
        final Field constField = clazz.getDeclaredField(matcher.group(2));
        return constField.get(clazz);
    }
}