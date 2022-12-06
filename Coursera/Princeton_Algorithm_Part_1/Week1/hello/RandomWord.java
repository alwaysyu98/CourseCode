import edu.princeton.cs.algs4.StdIn;
import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdOut;

/**
 * @description: Week1
 *
 * @className: RandomWord
 * @author: Jiri Yu
 * @date: 2021/10/21
 */
public class RandomWord {
    public static void main(String[] args) {
        int i = 0;
        String champion = "";
        while (!StdIn.isEmpty()) {
            ++i;
            String tmp = StdIn.readString();
            if (StdRandom.bernoulli(1.0/i)) {
                champion = tmp;
            }
        }
        StdOut.println(champion);
    }
}
