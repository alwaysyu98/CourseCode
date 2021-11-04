/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/03
 **************************************************************************** */

import edu.princeton.cs.algs4.StdIn;
import edu.princeton.cs.algs4.StdRandom;

import java.util.Iterator;

public class Permutation {
    public static void main(String[] args) {
        int k = Integer.parseInt(args[0]), ctn = 0;
        RandomizedQueue<String> randomizedQueue = new RandomizedQueue<>();
        while (!StdIn.isEmpty()) {
            String item = StdIn.readString();
            ctn++;
            if (ctn <= k) {
                randomizedQueue.enqueue(item);
            } else {
                if (StdRandom.bernoulli((double) k / ctn)) {
                    randomizedQueue.dequeue();
                    randomizedQueue.enqueue(item);
                }
            }
        }

        Iterator<String> iterator = randomizedQueue.iterator();
        while (iterator.hasNext()) {
            System.out.println(iterator.next());
        }
    }
}
