package de.mknblch.nolisp.core.interpreter.parser.lexer;

/**
 * @author mknblch
 */
public class StringCutter {

    private String str;
    private int offset;
    private int prev;

    public StringCutter() {
    }

    public StringCutter(String str) {
        this.str = str;
        reset();
    }

    public void setString(String str) {
        this.str = str;
        reset();
    }

    public String getString() {
        return str;
    }

    public void reset() {
        offset = 0;
        prev = 0;
    }

    public boolean hasNext() {
        return offset < str.length();
    }

    public String getToken() {
        return str.substring(prev, offset);
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
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

    public char popChar() {
        return str.charAt(offset++);
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
     * @param a single char for comparision
     * @param c set of chars
     * @return true if a is element of c. false otherwise
     */
    public static boolean E(char a, char[]... c) {
        for (char[] chs : c) {
            for (int i = 0; i < chs.length; i++) {
                if (a == chs[i]) return true;
            }
        }
        return false;
    }
}
