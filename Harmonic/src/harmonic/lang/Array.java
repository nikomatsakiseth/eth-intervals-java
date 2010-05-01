package harmonic.lang;

public interface Array<E> {
    E get(int index);
    void set(int index, E value);
    int length();
}