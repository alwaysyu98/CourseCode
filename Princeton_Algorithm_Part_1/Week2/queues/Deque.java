/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/03
 **************************************************************************** */

import java.util.Iterator;

public class Deque<Item> implements Iterable<Item> {
    private Node first, last;
    private int size;

    private class Node {
        Item item;
        Node next, prev;

        public Node(Item item) {
            this.item = item;
            next = null;
            prev = null;
        }
    }

    // construct an empty deque
    public Deque() {
        first = null;
        last = null;
        size = 0;
    }

    // is the deque empty?
    public boolean isEmpty() {
        return size == 0;
    }

    // return the number of items on the deque
    public int size() {
        return size;
    }

    // add the item to the front
    public void addFirst(Item item) {
        if (item == null) {
            throw new IllegalArgumentException();
        }
        Node oldFirst = first;
        first = new Node(item);
        if (isEmpty()) {
            last = first;
        } else {
            first.next = oldFirst;
            oldFirst.prev = first;
        }
        size++;
    }

    // add the item to the back
    public void addLast(Item item) {
        if (item == null) {
            throw new IllegalArgumentException();
        }
        Node oldLast = last;
        last = new Node(item);
        if (isEmpty()) {
            first = last;
        } else {
            oldLast.next = last;
            last.prev = oldLast;
        }
        size++;
    }

    // remove and return the item from the front
    public Item removeFirst() {
        if (isEmpty()) {
            throw new java.util.NoSuchElementException();
        }
        size--;
        Item item = first.item;
        first = first.next;
        if (first != null) {
            first.prev = null;
        } else {
            last = null;
        }
        return item;
    }

    // remove and return the item from the back
    public Item removeLast() {
        if (isEmpty()) {
            throw new java.util.NoSuchElementException();
        }
        size--;
        Item item = last.item;
        last = last.prev;
        if (last != null) {
            last.next = null;
        } else {
            first = null;
        }
        return item;
    }

    // return an iterator over items in order from front to back
    public Iterator<Item> iterator() {
        return new ItemIterator();
    }

    private class ItemIterator implements Iterator<Item> {
        private Node current = first;

        public boolean hasNext() {
            return current != null;
        }

        public Item next() {
            if (!hasNext()) {
                throw new java.util.NoSuchElementException();
            }
            Item item = current.item;
            current = current.next;
            return item;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    // unit testing (required)
    public static void main(String[] args) {
        Deque<Integer> deque = new Deque<>();
        System.out.println("size: " + deque.size());
        System.out.println("empty?: " + deque.isEmpty());
        deque.addFirst(1);
        deque.addFirst(2);
        deque.addFirst(3);
        // [3, 2, 1]
        Iterator<Integer> iterator = deque.iterator();
        while (iterator.hasNext()) {
            System.out.print(iterator.next() + " ");
        }
        System.out.print("\n");
        System.out.println("size: " + deque.size());
        System.out.println("empty?: " + deque.isEmpty());
        deque.addLast(4);
        deque.addLast(5);
        deque.addLast(6);
        // [3, 2, 1, 4, 5, 6]
        System.out.println("---------");
        for (Integer i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------");
        System.out.println("size: " + deque.size());
        System.out.println("empty?: " + deque.isEmpty());
        deque.removeFirst();
        // [2, 1, 4, 5, 6]
        deque.removeFirst();
        // [1, 4, 5, 6]
        System.out.println("---------");
        for (Integer i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------");

        deque.removeLast();
        // [1, 4, 5]
        deque.removeLast();
        // [1, 4]
        System.out.println("---------");
        for (Integer i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------");

        System.out.println(deque.size());
        System.out.println(deque.isEmpty());
        deque.removeLast();
        deque.removeLast();
        System.out.println("---------");
        for (Integer i : deque) {
            System.out.print(i + " ");
        }
        System.out.println("\n---------");
        // throw exception
        deque.removeLast();
    }

}
