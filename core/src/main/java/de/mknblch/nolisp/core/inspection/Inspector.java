package de.mknblch.nolisp.core.inspection;

import de.mknblch.nolisp.core.common.TypeHelper;
import de.mknblch.nolisp.core.datatypes.ListStruct;

/**
 * @author mknblch
 */
public class Inspector {

    /**
     * performs deep search
     *
     * @param tree the root node
     * @param rule the rule to apply to the tree
     * @throws Exception if anything goes wrong
     */
    public static void inspectTree(ListStruct tree, InspectionRule rule) throws Exception {
        inspectTree(tree, rule, 0);
    }

    /**
     * clone the tree based on it's elements
     *
     * @param tree
     * @param rule
     * @return
     * @throws Exception
     */
    public static ListStruct cloneTree(ListStruct tree, ValueCloneRule rule) throws Exception {
        final ListStruct clone = new ListStruct();
        ListStruct temp = tree;
        while (temp != null) {
            final Object car = temp.car();
            if (TypeHelper.isList(car)) {
                clone.add(rule.clone(cloneTree((ListStruct) car, rule)));
            } else {
                clone.add(rule.clone(temp.car()));
            }
            temp = temp.cdr();
        }
        return clone;
    }

    /**
     * clone the tree based on the listStruct container and it's value
     *
     * @param tree
     * @param rule
     * @return
     * @throws Exception
     */
    public static ListStruct cloneTree(ListStruct tree, ContainerCloneRule rule) throws Exception {
        final ListStruct clone = new ListStruct();
        if (tree.isEmpty()) return clone;
        ListStruct temp = tree;
        while (temp != null) {
            final Object car = temp.car();
            if (TypeHelper.isList(car)) {
                clone.attach(rule.cloneSublist(cloneTree((ListStruct) car, rule)));
            } else {
                clone.attach(rule.cloneElement(temp.car()));
            }
            temp = temp.cdr();
        }
        return clone.cdr();
    }

    private static void inspectTree(ListStruct tree, InspectionRule rule, int depth) throws Exception {
        ListStruct temp = tree;
        while (temp != null) {
            final Object car = temp.car();
            if (TypeHelper.isList(car) && rule.follow(temp, (ListStruct) car, depth)) {
                if (rule.inspectSublists()) rule.inspect(temp, temp.car(), depth);
                inspectTree((ListStruct) car, rule, depth + 1);
            } else {
                rule.inspect(temp, temp.car(), depth);
            }
            temp = temp.cdr();
        }
    }
}
