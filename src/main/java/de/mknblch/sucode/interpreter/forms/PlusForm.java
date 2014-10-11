package de.mknblch.sucode.interpreter.forms;

import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.TypeHelper;
import de.mknblch.sucode.interpreter.environment.Environment;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 11.10.2014.
 */
public class PlusForm implements Form {

    @Override
    public Object eval(ListStruct args, Environment environment, Interpreter interpreter) throws EvaluationException {
        Double result = 0d;
        boolean anyDouble = false;
        for (Object arg : args) {
            final Object evaluated = interpreter.eval(arg, environment);
            if(evaluated instanceof Double) anyDouble = true;
            result = sum(result, evaluated);
        }
        if (anyDouble) {
            return result;
        }
        return TypeHelper.asInt(result);
    }

    @Override
    public String getSymbol() {
        return "+";
    }

    private static Double sum(Double result, Object evaluated) throws EvaluationException {
        if(evaluated instanceof Double) {
            return result + (Double) evaluated;
        }
        if(evaluated instanceof Integer) {
            return result + (Integer) evaluated;
        }
        throw new EvaluationException("Invalid type cast.");
    }
}
