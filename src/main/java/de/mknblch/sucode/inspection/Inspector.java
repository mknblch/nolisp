package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class Inspector {

    public static void inspect(ListStruct tree, ElementInspectionRule rule) {

        ListStruct temp = tree;

        while(temp != null) {
            final Object car = temp.car();
            if(isList(car)) {
                final ListStruct listStruct = (ListStruct) car;
                inspect(listStruct, rule);
            } else {
                rule.inspect(temp);
            }
            temp = temp.cdr();
        }
    }

    public static void inspect(ListStruct tree, SubListInspectionRule rule) {

        ListStruct temp = tree;

        while(temp != null) {
            final Object car = temp.car();
            if(isList(car)) {
                final ListStruct listStruct = (ListStruct) car;
                if (!rule.inspectSubList(temp)) continue; // TODO review
                inspect(listStruct, rule);
            } else {
                rule.inspect(temp);
            }
            temp = temp.cdr();
        }
    }
}
