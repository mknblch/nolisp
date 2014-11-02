package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class Inspector {


    public static void inspectList() {

    }

    /**
     * performs depth search
     * @param tree the root node
     * @param rule the rule to apply to the tree
     * @throws Exception if anything goes wrong
     */
    public static void inspectTree(ListStruct tree, TreeRule rule) throws Exception {
        inspectTree(tree, rule, 0);
    }

    public static void inspectTree(ListStruct tree, TreeRule rule, int depth) throws Exception {
        ListStruct temp = tree;

        while(temp != null) {
            final Object car = temp.car();
            if(isList(car) && rule.follow(temp, (ListStruct) car, depth)) {
                rule.inspect(temp, temp.car(), depth);
                inspectTree((ListStruct) car, rule, depth+1); // TODO review
            } else {
                rule.inspect(temp, temp.car(), depth);
            }
            temp = temp.cdr();
        }
    }
}
