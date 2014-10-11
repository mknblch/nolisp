package de.mknblch.sucode.interpreter.forms;

import de.mknblch.sucode.interpreter.EvaluationException;
import de.mknblch.sucode.interpreter.Interpreter;
import de.mknblch.sucode.interpreter.Environment;
import de.mknblch.sucode.parser.structs.ListStruct;

/**
 * Created by mknblch on 11.10.2014.
 */
public interface Form {

    public Object eval (ListStruct args, Environment environment, Interpreter interpreter) throws EvaluationException;

    public String getSymbol();
}
