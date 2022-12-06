/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/11
 **************************************************************************** */

import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.StdOut;

import java.util.Arrays;

public class BruteCollinearPoints {
    private LineSegment[] lineSegments;
    private Point[] points;

    // finds all line segments containing 4 points
    public BruteCollinearPoints(Point[] points) {
        if (points == null) throw new IllegalArgumentException();

        for (int i = 0; i < points.length; ++i) {
            if (points[i] == null) throw new IllegalArgumentException();
        }

        this.points = new Point[points.length];
        for (int i = 0; i < points.length; ++i) {
            this.points[i] = points[i];
        }

        Arrays.sort(this.points);
        for (int i = 0; i < points.length; ++i) {
            if (i != 0 && this.points[i].compareTo(this.points[i-1]) == 0) throw new IllegalArgumentException();
        }

        // call segments
        setLineSegments();
    }

    // the number of line segments
    public int numberOfSegments() {
        if (lineSegments == null) {
            return 0;
        }
        return lineSegments.length;
    }

    // the line segments
    private void setLineSegments() {
        int pointsLen = points.length;
        if (pointsLen == 1) {
            lineSegments = new LineSegment[0];
            return;
        }

        int size = 0;
        for (int p = 0; p < pointsLen; ++p) {
            for (int q = p+1; q < pointsLen; ++q) {
                for (int r = q+1; r < pointsLen; ++r) {
                    for (int s = r+1; s < pointsLen; ++s) {
                        double slopePQ = points[p].slopeTo(points[q]), slopePR = points[p].slopeTo(points[r]), slopePS = points[p].slopeTo(points[s]);
                        if (slopePQ != slopePR) continue;
                        if (slopePQ != slopePS) continue;
                        size++;
                    }
                }
            }
        }

        lineSegments = new LineSegment[size];
        int index = 0;
        for (int p = 0; p < pointsLen; ++p) {
            for (int q = p+1; q < pointsLen; ++q) {
                for (int r = q+1; r < pointsLen; ++r) {
                    for (int s = r+1; s < pointsLen; ++s) {
                        double slopePQ = points[p].slopeTo(points[q]), slopePR = points[p].slopeTo(points[r]), slopePS = points[p].slopeTo(points[s]);
                        if (slopePQ != slopePR) continue;
                        if (slopePQ != slopePS) continue;
                        lineSegments[index++] = new LineSegment(points[p], points[s]);
                    }
                }
            }
        }
    }

    public LineSegment[] segments() {
        LineSegment[] result = new LineSegment[lineSegments.length];
        for (int i = 0; i < result.length; ++i) {
            result[i] = lineSegments[i];
        }
        return result;
    }

    public static void main(String[] args) {
        // read the n points from a file
        In in = new In(args[0]);
        int n = in.readInt();
        Point[] points = new Point[n];
        for (int i = 0; i < n; i++) {
            int x = in.readInt();
            int y = in.readInt();
            points[i] = new Point(x, y);
        }

        // draw the points
        StdDraw.enableDoubleBuffering();
        StdDraw.setXscale(0, 32768);
        StdDraw.setYscale(0, 32768);
        StdDraw.setPenRadius(0.01);
        for (Point p : points) {
            p.draw();
        }
        StdDraw.show();

        // print and draw the line segments
        StdDraw.setPenRadius();
        BruteCollinearPoints collinear = new BruteCollinearPoints(points);
        // LineSegment[] lineSegments1 = collinear.segments();
        // points[2] = new Point(1, 2);
        // points[3] = new Point(2, 4);
        // LineSegment[] lineSegments2 = collinear.segments();
        // assert lineSegments1 == lineSegments2;

        for (LineSegment segment : collinear.segments()) {
            StdOut.println(segment);
            segment.draw();
        }
        StdDraw.show();
    }
}
