/* *****************************************************************************
 *  Name: Jiri Yu
 *  Date: 2021/11/15
 **************************************************************************** */

import edu.princeton.cs.algs4.In;

import java.util.ArrayList;
import java.util.List;

public class Board {
    private int[][] tiles;
    private int n;

    // create a board from an n-by-n array of tiles,
    // where tiles[row][col] = tile at (row, col)
    public Board(int[][] tiles) {
        n = tiles.length;
        this.tiles = new int[n][n];
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                this.tiles[i][j] = tiles[i][j];
            }
        }
    }

    // string representation of this board
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(n + "\n");
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                stringBuilder.append(" " + tiles[i][j]);
            }
            stringBuilder.append("\n");
        }
        return stringBuilder.toString();
    }

    // board dimension n
    public int dimension() {
        return n;
    }

    // number of tiles out of place
    public int hamming() {
        int num = 0;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (tiles[i][j] == 0) {
                    continue;
                }
                int tmp = tiles[i][j] - 1;
                int x = tmp / n;
                int y = tmp % n;
                num += (i != x || j != y) ? 1 : 0;
            }
        }
        return num;
    }

    // sum of Manhattan distances between tiles and goal
    public int manhattan() {
        int num = 0;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (tiles[i][j] == 0) {
                    continue;
                }
                int tmp = (tiles[i][j] - 1 + n * n) % (n * n);
                int x = tmp / n;
                int y = tmp % n;
                num += Math.abs(i-x) + Math.abs((j-y));
            }
        }
        return num;
    }

    // is this board the goal board?
    public boolean isGoal() {
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (tiles[i][j] != (i * n + j + 1) % (n * n)) {
                    return false;
                }
            }
        }
        return true;
    }

    // does this board equal y?
    public boolean equals(Object y) {
        if (y == null || getClass() != y.getClass()) {
            return false;
        }

        if (this == y) {
            return true;
        }

        Board board = (Board) y;
        if (n != board.dimension()) {
            return false;
        }

        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (tiles[i][j] != board.tiles[i][j]) {
                    return false;
                }
            }
        }

        return true;
    }

    // all neighboring boards
    public Iterable<Board> neighbors() {
        List<Board> list = new ArrayList<>();
        int x = -1, y = -1;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (tiles[i][j] == 0) {
                    x = i;
                    y = j;
                    break;
                }
            }
        }

        int[] dirctions = {-1, 0, 1, 0, -1};
        for (int i = 0; i < dirctions.length - 1; ++i) {
            int nx = x + dirctions[i];
            int ny = y + dirctions[i+1];
            if (nx >= 0 && nx < n && ny >= 0 && ny < n) {
                Board board = new Board(tiles);
                exchange(board, new int[] { x, y }, new int[] { nx, ny });
                list.add(board);
            }
        }

        return list;
    }

    private void exchange(Board board, int[] a, int[] b) {
        int tmp = board.tiles[a[0]][a[1]];
        board.tiles[a[0]][a[1]] = board.tiles[b[0]][b[1]];
        board.tiles[b[0]][b[1]] = tmp;
    }

    // a board that is obtained by exchanging any pair of tiles
    public Board twin() {
        int[][] tmpTiles = new int[n][n];
        int[][] changes = new int[2][2];

        int idx = 0;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (idx < 2 && this.tiles[i][j] != 0) {
                    changes[idx++] = new int[] {i, j};
                }
                tmpTiles[i][j] = this.tiles[i][j];
            }
        }

        Board board = new Board(tmpTiles);
        exchange(board, changes[0], changes[1]);
        return board;
    }

    // unit testing (not graded)
    public static void main(String[] args) {
        // create initial board from file
        In in = new In(args[0]);
        int n = in.readInt();
        int[][] tiles = new int[n][n];
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
                tiles[i][j] = in.readInt();
        Board initial = new Board(tiles);
        System.out.println(initial.toString());

        System.out.println("Dimension:");
        System.out.println(initial.dimension());

        System.out.println("Manhattan:");
        System.out.println(initial.manhattan());

        System.out.println("hamming:");
        System.out.println(initial.hamming());

        System.out.println("twin:");
        System.out.println(initial.twin());

        System.out.println("Neighbors:");
        for (Board board : initial.neighbors()) {
            System.out.println(board);
        }
    }

}
