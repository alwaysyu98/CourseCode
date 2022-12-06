/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/03
 **************************************************************************** */

import edu.princeton.cs.algs4.StdRandom;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class RandomizedQueue<Item> implements Iterable<Item> {
    private Item[] items;
    private int size;

    // construct an empty randomized queue
    public RandomizedQueue() {
        items = (Item[]) new Object[1];
        size = 0;
    }

    // is the randomized queue empty?
    public boolean isEmpty() {
        return size == 0;
    }

    // return the number of items on the randomized queue
    public int size() {
        return size;
    }


    // add the item
    public void enqueue(Item item) {
        if (item == null) {
            throw new IllegalArgumentException();
        }
        Item[] oldItems = items;
        if (size == items.length) {
            items = (Item[]) new Object[items.length*2];
            for (int i = 0; i < size; ++i) {
                items[i] = oldItems[i];
            }
        }
        items[size++] = item;
    }

    // remove and return a random item
    public Item dequeue() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }

        int idx = StdRandom.uniform(size);
        Item item = items[idx];
        items[idx] = items[--size];

        Item[] oldItems = items;

        if (size == 0) {
            items = (Item[]) new Object[1];
        } else if (size == items.length/4) {
            items = (Item[]) new Object[items.length/2];
            for (int i = 0; i < size; ++i) {
                items[i] = oldItems[i];
            }
        }
        return item;
    }

    // return a random item (but do not remove it)
    public Item sample() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        return items[StdRandom.uniform(size)];
    }

    // return an independent iterator over items in random order
    public Iterator<Item> iterator() {
        return new ItemIterator();
    }

    private class ItemIterator implements Iterator<Item> {
        private int current;
        private Item[] refItems;

        public ItemIterator() {
            current = size;
            refItems = (Item[]) new Object[size];
            for (int i = 0; i < size; ++i) {
                refItems[i] = items[i];
            }
        }

        public boolean hasNext() {
            return current != 0;
        }

        public Item next() {
            if (current == 0) {
                throw new NoSuchElementException();
            }
            int idx = StdRandom.uniform(current);
            Item item = refItems[idx];
            refItems[idx] = refItems[--current];

            return item;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    // unit testing (required)
    public static void main(String[] args) {
        RandomizedQueue<Integer> deque = new RandomizedQueue<>();
        System.out.println("isEmpty: " + deque.isEmpty());
        System.out.println("size: " + deque.size());
        deque.enqueue(1);
        deque.enqueue(2);
        deque.enqueue(3);
        deque.enqueue(4);
        System.out.println("isEmpty: " + deque.isEmpty());
        System.out.println("size: " + deque.size());
        System.out.println("sample: " + deque.sample());

        Iterator<Integer> iterator = deque.iterator();
        System.out.println("---------------------");
        while (iterator.hasNext()) {
            System.out.print(iterator.next() + " ");
        }
        System.out.println("\n---------------------");

        int item;
        item = deque.dequeue();
        System.out.println("---------------------");
        for (int i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------------------");
        System.out.println("isEmpty: " + deque.isEmpty());
        System.out.println("size: " + deque.size());
        System.out.println("deleted item: " + item);

        item = deque.dequeue();
        System.out.println("---------------------");
        for (int i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------------------");
        System.out.println("isEmpty: " + deque.isEmpty());
        System.out.println("size: " + deque.size());
        System.out.println("deleted item: " + item);

        item = deque.dequeue();
        System.out.println("---------------------");
        for (int i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------------------");
        System.out.println("isEmpty: " + deque.isEmpty());
        System.out.println("size: " + deque.size());
        System.out.println("deleted item: " + item);

        item = deque.dequeue();
        System.out.println("---------------------");
        for (int i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------------------");
        System.out.println("isEmpty: " + deque.isEmpty());
        System.out.println("size: " + deque.size());
        System.out.println("deleted item: " + item);

        deque.enqueue(1);
        deque.enqueue(2);
        deque.enqueue(3);
        deque.enqueue(4);
        System.out.println("isEmpty: " + deque.isEmpty());
        System.out.println("size: " + deque.size());

    }

}