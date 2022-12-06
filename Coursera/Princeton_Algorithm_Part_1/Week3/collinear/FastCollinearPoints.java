/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/12
 **************************************************************************** */

import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.StdOut;

import java.util.Arrays;

public class FastCollinearPoints {
    private Point[] points;
    private LineSegment[] lineSegments;

    // finds all line segments containing 4 or more points
    public FastCollinearPoints(Point[] points) {
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

    private void setLineSegments() {
        int pointsLen = points.length;
        int size = 0;

        if (pointsLen == 1) {
            lineSegments = new LineSegment[0];
            return;
        }

        Point[] tmpPoints = new Point[pointsLen-1];

        for (int i = 0; i < pointsLen; ++i) {
            for (int j = 0, index = 0; j < pointsLen; ++j) {
                if (j == i) continue;
                tmpPoints[index++] = points[j];
            }
            Arrays.sort(tmpPoints, points[i].slopeOrder());

            double prevSlope = tmpPoints[0].slopeTo(points[i]);
            for (int j = 0; j < tmpPoints.length;) {
                int ct = 0;
                boolean flag = true;
                while (j < tmpPoints.length && tmpPoints[j].slopeTo(points[i]) == prevSlope) {
                    if (points[i].compareTo(tmpPoints[j]) >= 0) flag = false;
                    ++j;
                    ++ct;
                }

                // update prevSlope
                if (j < tmpPoints.length) {
                    prevSlope = tmpPoints[j].slopeTo(points[i]);
                }

                if (ct >= 3 && flag) {
                    ++size;
                }
            }
        }

        lineSegments = new LineSegment[size];
        int idx = 0;
        for (int i = 0; i < pointsLen; ++i) {
            for (int j = 0, index = 0; j < pointsLen; ++j) {
                if (j == i) continue;
                tmpPoints[index++] = points[j];
            }
            Arrays.sort(tmpPoints, points[i].slopeOrder());

            double prevSlope = tmpPoints[0].slopeTo(points[i]);
            for (int j = 0; j < tmpPoints.length;) {
                int ct = 0;
                boolean flag = true;
                while (j < tmpPoints.length && tmpPoints[j].slopeTo(points[i]) == prevSlope) {
                    if (points[i].compareTo(tmpPoints[j]) >= 0) flag = false;
                    ++j;
                    ++ct;
                }

                // update prevSlope
                if (j < tmpPoints.length) {
                    prevSlope = tmpPoints[j].slopeTo(points[i]);
                }

                if (ct >= 3 && flag) {
                    lineSegments[idx++] = new LineSegment(points[i], tmpPoints[j-1]);
                }
            }
        }
    }

    // the line segments
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
        FastCollinearPoints collinear = new FastCollinearPoints(points);
        for (LineSegment segment : collinear.segments()) {
            StdOut.println(segment);
            segment.draw();
        }
        StdDraw.show();
    }
}
