package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class Inspector {

    public static void inspect(ListStruct tree, InspectionRule rule) {

        ListStruct temp = tree;

        while(temp != null) {
            final Object car = temp.car();
            if(isList(car) && rule.follow(temp)) {
                inspect((ListStruct) car, rule); // TODO review
            } else {
                rule.inspect(temp, temp.car());
            }
            temp = temp.cdr();
        }
    }
}
