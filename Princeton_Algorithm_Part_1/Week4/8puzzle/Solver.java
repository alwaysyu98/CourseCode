/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/16
 **************************************************************************** */

import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.MinPQ;
import edu.princeton.cs.algs4.Stack;
import edu.princeton.cs.algs4.StdOut;

import java.util.Comparator;

public class Solver {
    private int minMoves;
    private Stack<Board> solutions;

    private class AStarNode {
        public Board board;
        public AStarNode prevAStarNode;
        public int g, h, f;
        public AStarNode(Board board, AStarNode prevAStarNode, int g) {
            this.board = board;
            this.prevAStarNode = prevAStarNode;
            this.g = g;
            this.h = board.manhattan();
            this.f = this.g + this.h;
        }
    }

    private class ComparatorBoard implements Comparator<AStarNode> {
        public int compare(AStarNode aStarNode1, AStarNode aStarNode2) {
            if (aStarNode1.f < aStarNode2.f) return -1;
            if (aStarNode1.f > aStarNode2.f) return +1;
            // return aStarNode1.g - aStarNode2.g;
            return 0;
        }
    }

    // find a solution to the initial board (using the A* algorithm)
    public Solver(Board initial) {
        if (initial == null) throw new IllegalArgumentException();

        MinPQ<AStarNode> minPQ, twinMinPQ;

        // construct board, minMoves and solutions
        this.solutions = new Stack<>();
        this.minMoves = -1;

        // assign minPQ
        minPQ = new MinPQ<AStarNode>(new ComparatorBoard());
        twinMinPQ = new MinPQ<AStarNode>(new ComparatorBoard());

        // A-star, before start
        minPQ.insert(new AStarNode(initial, null, 0));
        twinMinPQ.insert(new AStarNode(initial.twin(), null, 0));

        // f = g + h(Manhattan / Hamming)
        // A-star, start
        AStarNode endNode = null;
        while (!minPQ.isEmpty()) {
            // System.out.println(aStarNode.board);
            // System.out.println("f:" + aStarNode.f + ", g:" + aStarNode.g + ", h" + aStarNode.h);

            // is solved? yes -> break
            AStarNode aStarNode = minPQ.delMin();
            if (aStarNode.h == 0) {
                minMoves = aStarNode.g;
                endNode = aStarNode;
                break;
            }

            for (Board neighbor : aStarNode.board.neighbors()) {
                if (aStarNode.prevAStarNode == null || !aStarNode.prevAStarNode.board.equals(neighbor)) {
                    minPQ.insert(new AStarNode(neighbor, aStarNode, aStarNode.g+1));
                }
            }

            if (twinMinPQ.isEmpty()) {
                continue;
            }
            AStarNode twinAStarNode = twinMinPQ.delMin();
            if (twinAStarNode.h == 0) {
                minMoves = -1;
                endNode = null;
                break;
            }
            for (Board twinNeighbor : twinAStarNode.board.neighbors()) {
                if (twinAStarNode.prevAStarNode == null || !twinAStarNode.prevAStarNode.board.equals(twinNeighbor)) {
                    twinMinPQ.insert(new AStarNode(twinNeighbor, twinAStarNode, twinAStarNode.g+1));
                }
            }
        }


        // include the initial path
        if (endNode != null) {
            for (int i = minMoves; i >= 0; --i) {
                solutions.push(endNode.board);
                endNode = endNode.prevAStarNode;
            }
        }
    }

    // is the initial board solvable? (see below)
    public boolean isSolvable() {
        return minMoves != -1;
    }

    // min number of moves to solve initial board; -1 if unsolvable
    public int moves() {
        return minMoves;
    }

    // sequence of boards in a shortest solution; null if unsolvable
    public Iterable<Board> solution() {
        if (!isSolvable()) {
            return null;
        }

        return solutions;
    }

    // test client (see below)
    public static void main(String[] args) {
        // create initial board from file
        In in = new In(args[0]);
        int n = in.readInt();
        int[][] tiles = new int[n][n];
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
                tiles[i][j] = in.readInt();
        Board initial = new Board(tiles);

        // solve the puzzle
        Solver solver = new Solver(initial);

        // print solution to standard output
        if (!solver.isSolvable())
            StdOut.println("No solution possible");
        else {
            StdOut.println("Minimum number of moves = " + solver.moves());
            for (Board board : solver.solution())
                StdOut.println(board);
        }
    }
}
