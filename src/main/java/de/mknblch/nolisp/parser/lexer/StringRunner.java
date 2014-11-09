package de.mknblch.nolisp.parser.lexer;

/**
 * @author mknblch
 */
public class StringRunner {

    private String str;
    private int offset;
    private int prev;

    public StringRunner() {
    }

    public StringRunner(String str) {
        this.str = str;
        reset();
    }

    public void reset() {
        offset = 0;
        prev = 0;
    }

    public String getToken() {
        return str.substring(prev, offset);
    }
    public boolean hasNext() {
        return offset < str.length();
    }

    public int getOffset() {
        return offset;
    }

    public void setString(String str) {
        this.str = str;
        reset();
    }
    /**
     * increment offset until any char OTHER THEN charsToSkip is found
     *
     * @param charsToSkip these chars should be ignored
     */
    public void skip(char[]... charsToSkip) {
        prev = offset;
        for (int i = offset; i < str.length(); i++) {
            if (E(str.charAt(offset), charsToSkip)) {
                offset++;
            } else {
                return;
            }
        }
    }

    /**
     * increment offset until any of charsToStop is found
     *
     * @param charsToStop chars to search for
     */
    public void until(char[]... charsToStop) {
        prev = offset;
        for (int i = offset; i < str.length(); i++) {
            if (!E(str.charAt(offset), charsToStop)) {
                offset++;
            } else {
                return;
            }
        }
    }

    /**
     * determine if char a is element of c
     *
     * @param a single char for comparision
     * @param c set of chars
     * @return true if a is element of c. false otherwise
     */
    public static boolean E (char a, char[]... c) {
        for (char[] chs : c) {
            for (int i = 0; i < chs.length; i++) {
                if (a == chs[i]) return true;
            }
        }
        return false;
    }
}
