package de.mknblch.nolisp.minimal;

import de.mknblch.nolisp.common.Expectations;
import de.mknblch.nolisp.common.FormatHelper;
import de.mknblch.nolisp.common.TypeHelper;
import de.mknblch.nolisp.interpreter.Context;
import de.mknblch.nolisp.interpreter.EvaluationException;
import de.mknblch.nolisp.interpreter.Interpreter;
import de.mknblch.nolisp.parser.Parser;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.scanner.Define;
import de.mknblch.nolisp.scanner.Special;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.net.URL;

import static de.mknblch.nolisp.common.TypeHelper.*;

/**
 * @author mknblch
 */
public class BasicForms {

    private static final Parser PARSER = new Parser();



    @Define("progn") // (progn 1 2 3) => 3
    public static Object progn(ListStruct args) {
        if (null == args) {
            return null;
        }
        return args.last().car();
    }

    @Special
    @Define(value = "fori") // (fori (start:INT end:INT [step:INT | 1] ) <form>)
    public static Object fori(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final ListStruct loopArgs = asList(args.car());
        final int from = asInt(interpreter.eval(loopArgs.car(), context)); // from
        final int to = asInt(interpreter.eval(loopArgs.cadr(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.caddr(), context); // step if not null
        final int step = isInt(stepRaw) ? asInt(stepRaw) : 1;
        final Object form = args.cadr();
        Object result = null;
        for (int i = from; i < to; i += step) {
            result = interpreter.eval(form, context);
        }
        return result;
    }


    @Special
    @Define(value = "for") // (fori (i:SYM start:INT end:INT [step:INT | 1] ) (<form>+))
    public static Object forForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Context localScope = context.derive();
        final ListStruct loopArgs = asList(args.car());
        final String literal = getSymbolLiteral(loopArgs.car()); // sym
        final int from = asInt(interpreter.eval(loopArgs.cadr(), context)); // from
        final int to = asInt(interpreter.eval(loopArgs.caddr(), context)); // to
        final Object stepRaw = interpreter.eval(loopArgs.cadddr(), context); // step if not null
        final int step = isInt(stepRaw) ? asInt(stepRaw) : 1;
        final ListStruct forms = asList(args.cadr());
        Object result = null;
        for (int i = from; i < to; i += step) {
            localScope.bind(literal, i);
            for (Object form : forms) {
                result = interpreter.eval(form, localScope);
            }
        }
        return result;
    }


    @Special
    @Define(value = "while") // (while form (form+))
    public static Object whileForm(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final Object condition = args.car();
        final ListStruct forms = asList(args.cadr());
        ListStruct r = null;
        while (asBoolean(interpreter.eval(condition, context))) {
            r = interpreter.evalEach(forms, context);
        }
        return null != r ? r.last().car() : null;
    }


    @Special
    @Define({"load", "load-file"}) // (load "file://abc.nolisp" )
    public static Object loadFile(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final File file = new File(asString(interpreter.eval(args.car(), context)));
        return interpreter.evalEach(PARSER.parse(new BufferedInputStream(new FileInputStream(file))), context);
    }

    @Special
    @Define("load-url") // (load "classpath:de/mknblch/nolisp/resources/abc.nolisp" )
    public static Object loadUrl(Interpreter interpreter, Context context, ListStruct args) throws Exception {
        final URL url = new URL(asString(interpreter.eval(args.car(), context)));
        return interpreter.evalEach(PARSER.parse(new BufferedInputStream(url.openStream())), context);
    }
}