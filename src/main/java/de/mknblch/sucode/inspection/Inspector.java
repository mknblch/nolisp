package de.mknblch.sucode.inspection;

import de.mknblch.sucode.ast.ListStruct;

import static de.mknblch.sucode.helper.TypeHelper.*;

/**
 * @author mknblch
 */
public class Inspector {


    public static void inspectList(ListStruct list, ListRule rule) throws Exception {

        ListStruct temp = list;
        while (temp != null) {
            if(!rule.inspect(temp, temp.car())) break;
            temp = temp.cdr();
        }
    }

    /**
     * performs deep search
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
                if(rule.inspectSublists()) rule.inspect(temp, temp.car(), depth);
                inspectTree((ListStruct) car, rule, depth + 1); // TODO review
            } else {
                rule.inspect(temp, temp.car(), depth);
            }
            temp = temp.cdr();
        }
    }

    public static ListStruct cloneTree(ListStruct tree, CloneRule rule) throws Exception {

        final ListStruct clone = new ListStruct();
        ListStruct temp = tree;

        while(temp != null) {
            final Object car = temp.car();
            if(isList(car)) {
                clone.add(rule.clone(cloneTree((ListStruct) car, rule)));
            } else {
                clone.add(rule.clone(temp.car()));
            }
            temp = temp.cdr();
        }

        return clone;
    }

//    public static void cloneTree(ListStruct tree, ListStruct clone, CloneRule rule) throws Exception {
//
//        ListStruct temp = tree;
//
//        while(temp != null) {
//            final Object car = temp.car();
//            if(isList(car) && rule.follow((ListStruct) car)) {
//                final ListStruct subList = new ListStruct();
//                clone.add(subList);
//                cloneTree((ListStruct) car, subList, rule);
//            } else {
//                clone.add(rule.clone(temp.car()));
//            }
//            temp = temp.cdr();
//        }
//    }
}
