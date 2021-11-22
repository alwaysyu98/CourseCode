/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/22
 **************************************************************************** */

import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.Point2D;
import edu.princeton.cs.algs4.RectHV;
import edu.princeton.cs.algs4.StdDraw;

import java.util.ArrayList;
import java.util.List;


public class KdTree {
    private Node root;
    private int size;

    private class Node {
        public boolean currentLevel;
        public Point2D point2D;
        public Node left, right;
        public RectHV rectHV;

        public Node(Point2D point2D, boolean level, RectHV rectHV) {
            this.currentLevel = level;
            this.point2D = point2D;
            this.rectHV = rectHV;
            this.left = null;
            this.right = null;
        }
    }

    // construct an empty set of points
    public KdTree() {
        root = null;
        size = 0;
    }

    // is the set empty?
    public boolean isEmpty() {
        return size == 0;
    }

    // number of points in the set
    public int size() {
        return size;
    }

    // add the point to the set (if it is not already in the set)
    public void insert(Point2D p) {
        if (p == null) throw new IllegalArgumentException();

        if (root == null) {
            root = new Node(p, true, new RectHV(0.0, 0.0, 1.0, 1.0));
            size += 1;
            return;
        }

        Node currentNode = root;
        while (true) {
            if (currentNode.point2D.compareTo(p) == 0) return;

            double xmin = currentNode.rectHV.xmin();
            double xmax = currentNode.rectHV.xmax();
            double ymin = currentNode.rectHV.ymin();
            double ymax = currentNode.rectHV.ymax();

            if (currentNode.currentLevel) {
                if (p.x() < currentNode.point2D.x()) {
                    if (currentNode.left == null) {
                        RectHV newRectHV = new RectHV(xmin, ymin, currentNode.point2D.x(), ymax);
                        currentNode.left = new Node(p, false, newRectHV);
                        size += 1;
                        return;
                    }
                    currentNode = currentNode.left;
                } else {
                    if (currentNode.right == null) {
                        RectHV newRectHV = new RectHV(currentNode.point2D.x(), ymin, xmax, ymax);
                        currentNode.right = new Node(p, false, newRectHV);
                        size += 1;
                        return;
                    }
                    currentNode = currentNode.right;
                }
            } else {
                if (p.y() < currentNode.point2D.y()) {
                    if (currentNode.left == null) {
                        RectHV newRectHV = new RectHV(xmin, ymin, xmax, currentNode.point2D.y());
                        currentNode.left = new Node(p, true, newRectHV);
                        size += 1;
                        return;
                    }
                    currentNode = currentNode.left;
                } else {
                    if (currentNode.right == null) {
                        RectHV newRectHV = new RectHV(xmin, currentNode.point2D.y(), xmax, ymax);
                        currentNode.right = new Node(p, true, newRectHV);
                        size += 1;
                        return;
                    }
                    currentNode = currentNode.right;
                }
            }
        }
    }

    // does the set contain point p?
    public boolean contains(Point2D p) {
        if (p == null) throw new IllegalArgumentException();
        if (root == null) return false;

        Node currentNode = root;
        while (currentNode != null) {
            if (currentNode.point2D.compareTo(p) == 0) return true;

            if (currentNode.currentLevel) {
                if (p.x() < currentNode.point2D.x()) { currentNode = currentNode.left; }
                else { currentNode = currentNode.right; }
            } else {
                if (p.y() < currentNode.point2D.y()) { currentNode = currentNode.left; }
                else { currentNode = currentNode.right; }
            }
        }
        return false;
    }

    // draw all points to standard draw
    public void draw() {
        if (root == null) throw new IllegalArgumentException();
        draw(root);
    }

    // root: current node, x: [xl, xr], y: [yl, yr]
    private void draw(Node currentNode) {
        if (currentNode == null) return;

        StdDraw.setPenRadius(0.02);
        StdDraw.setPenColor(StdDraw.BLACK);
        currentNode.point2D.draw();
        double x0 = currentNode.rectHV.xmin();
        double y0 = currentNode.rectHV.ymin();
        double x1 = currentNode.rectHV.xmax();
        double y1 = currentNode.rectHV.ymax();

        if (currentNode.currentLevel) {
            // use x to compare, but draw vertically
            StdDraw.setPenRadius(0.005);
            StdDraw.setPenColor(StdDraw.RED);
            StdDraw.line(currentNode.point2D.x(), y0, currentNode.point2D.x(), y1);
            draw(currentNode.left);
            draw(currentNode.right);
        } else {
            // use y to compare, so draw horizontally
            StdDraw.setPenRadius(0.005);
            StdDraw.setPenColor(StdDraw.BLUE);
            StdDraw.line(x0, currentNode.point2D.y(), x1, currentNode.point2D.y());
            draw(currentNode.left);
            draw(currentNode.right);
        }
    }

    // all points that are inside the rectangle (or on the boundary)
    public Iterable<Point2D> range(RectHV rect) {
        if (rect == null) throw new IllegalArgumentException();
        List<Point2D> point2DList = new ArrayList<>();
        range(root, rect, point2DList);
        return point2DList;
    }

    private void range(Node currentNode, RectHV rect, List<Point2D> point2DList) {
        if (currentNode == null) return;
        if (!currentNode.rectHV.intersects(rect)) return;

        if (rect.contains(currentNode.point2D)) point2DList.add(currentNode.point2D);

        range(currentNode.left, rect, point2DList);
        range(currentNode.right, rect, point2DList);
    }

    // a nearest neighbor in the set to point p; null if the set is empty
    public Point2D nearest(Point2D p) {
        if (p == null) throw new IllegalArgumentException();
        if (isEmpty()) return null;

        return nearest(root, p, root.point2D);
    }

    private Point2D nearest(Node currentNode, Point2D p, Point2D champion) {
        if (currentNode == null) return champion;

        double championDist = champion.distanceSquaredTo(p);
        double distToRect = currentNode.rectHV.distanceSquaredTo(p);

        // closer than the rectangle
        if (!currentNode.rectHV.contains(p) && distToRect > championDist) {
            return champion;
        }

        if (champion.distanceSquaredTo(p) > currentNode.point2D.distanceSquaredTo(p)) {
            champion = currentNode.point2D;
        }

        if ((currentNode.currentLevel && p.x() < currentNode.point2D.x()) || (!currentNode.currentLevel && p.y() < currentNode.point2D.y())) {
            Point2D leftMinNode = nearest(currentNode.left, p, champion);
            return nearest(currentNode.right, p, leftMinNode);
        } else {
            Point2D rightMinNode = nearest(currentNode.right, p, champion);
            return nearest(currentNode.left, p, rightMinNode);
        }
    }

    // unit testing of the methods (optional)
    public static void main(String[] args) {
        // initialize the two data structures with point from file
        String filename = args[0];
        In in = new In(filename);
        KdTree kdTree = new KdTree();
        while (!in.isEmpty()) {
            double x = in.readDouble();
            double y = in.readDouble();
            Point2D p = new Point2D(x, y);
            kdTree.insert(p);
        }

        // process nearest neighbor queries
        StdDraw.enableDoubleBuffering();
        while (true) {

            // the location (x, y) of the mouse
            double x = StdDraw.mouseX();
            double y = StdDraw.mouseY();
            Point2D query = new Point2D(0.726, 0.439);

            // draw all of the points
            StdDraw.clear();
            StdDraw.setPenColor(StdDraw.BLACK);
            StdDraw.setPenRadius(0.01);
            kdTree.draw();

            // draw in red the nearest neighbor (using brute-force algorithm)
            StdDraw.setPenRadius(0.03);
            StdDraw.setPenColor(StdDraw.RED);
            kdTree.nearest(query).draw();
            StdDraw.setPenRadius(0.02);

            StdDraw.show();
            StdDraw.pause(40);
        }
    }
}
