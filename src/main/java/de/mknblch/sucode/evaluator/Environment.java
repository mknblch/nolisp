package de.mknblch.sucode.evaluator;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by mknblch on 10.10.2014.
 */
public class Environment extends HashMap<String, Object> {
    public Environment(int initialCapacity) {
        super(initialCapacity);
    }

    public Environment() {
    }

    public Environment(Map<? extends String, ?> m) {
        super(m);
    }
}
