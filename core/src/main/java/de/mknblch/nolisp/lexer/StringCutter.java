package de.mknblch.nolisp.lexer;

/**
 * @author mknblch
 */
public class StringCutter {

    private String str;
    private int offset;
    private int prev;

    public StringCutter() {
    }

    public StringCutter setString(String str) {
        this.str = str;
        reset();
        return this;
    }

    public String getString() {
        return str;
    }

    public void reset() {
        offset = 0;
        prev = 0;
    }

    /**
     * @return false if end reached (offset >= string length); true otherwise
     */
    public boolean hasNext() {
        return offset < str.length();
    }

    /**
     * returns substring from previous hit point offset to current hit point offset
     * @return
     */
    public String getLiteral() {
        return str.substring(prev, offset);
    }

    public char charAtOffset() {
        return str.charAt(offset);
    }

    public void sync() {
        prev = offset;
    }

    public void inc() {
        offset++;
    }
    public void inc(int i) {
        offset += i;
    }

    public char popChar() {
        return str.charAt(offset++);
    }

    /**
     * returns true if the next chars are equal to the chars in la.
     * @param la chars to match
     * @return true if
     */
    public boolean lookAheadEquals(char[] la) {
        if(la.length + offset >= str.length()) return false;
        for (int i = 0; i < la.length; i++) {
            if (la[i] != str.charAt(offset+i)) return false;
        }
        return true;
    }

    /**
     * increment offset until any char OTHER THEN charsToSkip is found
     *
     * @param charsToSkip these chars should be ignored
     */
    public void skip(char[]... charsToSkip) {
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
     * @param a single char for comparison
     * @param c set of chars
     * @return true if a is element of c. false otherwise
     */
    private static boolean E(char a, char[]... c) {
        for (char[] chs : c) {
            for (int i = 0; i < chs.length; i++) {
                if (a == chs[i]) return true;
            }
        }
        return false;
    }
}
