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
            if(isList(car)) {
                final ListStruct listStruct = (ListStruct) car;
                rule.inspectList(listStruct);
                inspect(listStruct, rule);
            } else {
                rule.inspect(temp);
            }
            temp = temp.cdr();
        }
    }
}
