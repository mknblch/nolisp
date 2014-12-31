package de.mknblch.nolisp.features.minimal.list;

import de.mknblch.nolisp.datatypes.Form;
import de.mknblch.nolisp.datatypes.ListStruct;
import de.mknblch.nolisp.generator.annotations.Define;

import java.util.Collection;

/**
 * @author mknblch
 */
@Define({"element?", "elementof"})
public class IsElementForm implements Form {
    @Override
    public Object eval(ListStruct args) throws Exception {

        final Object haystack = args.car();
        final Object needle = args.cadr();

        if(haystack instanceof ListStruct) {
            for (Object o : (ListStruct) haystack) {
                if (needle.equals(o)) return true;
            }
        } else if (haystack instanceof Collection) {
            return ((Collection) haystack).contains(needle);
        }

        return false;
    }
}
