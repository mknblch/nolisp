package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class Inspector {



    public static void inspect(ListStruct tree, Rule rule) throws Exception {

        inspect(tree, rule, 0);
    }

    public static void inspect(ListStruct tree, Rule rule, int depth) throws Exception {
        ListStruct temp = tree;

        while(temp != null) {
            final Object car = temp.car();
            if(isList(car) && rule.follow(temp, (ListStruct) car, depth)) {
                rule.inspect(temp, temp.car(), depth);
                inspect((ListStruct) (ListStruct) car, (Rule) rule, depth+1); // TODO review
            } else {
                rule.inspect(temp, temp.car(), depth);
            }
            temp = temp.cdr();
        }
    }
}
