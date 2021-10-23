/* *****************************************************************************
 *  Name:              Jiri Yu
 *  Last modified:     10/23/2021
 **************************************************************************** */

import edu.princeton.cs.algs4.WeightedQuickUnionUF;

public class Percolation {
    private int n, numOpenSites;
    private boolean[][] systems;
    private WeightedQuickUnionUF weightedQuickUnionUF, weightedQuickUnionUF2;
    private final int BOTTOM, TOP;

    // creates n-by-n grid, with all sites initially blocked
    public Percolation(int n){
        if (n <= 0) {
            throw new IllegalArgumentException();
        }

        this.n = n;
        this.numOpenSites = 0;
        this.TOP = n*n;
        this.BOTTOM = n*n+1;

        // initially, systems are all blocked (full).
        systems = new boolean[n][n];

        // initial them
        for (int i=0; i<n; ++i){
            for (int j=0; j<n; ++j){
                systems[i][j] = false;
            }
        }

        // n*n -> top, n*n+1 ->bottom
        weightedQuickUnionUF = new WeightedQuickUnionUF(n*n+2);
        weightedQuickUnionUF2 = new WeightedQuickUnionUF(n*n+1);

        for (int i=0; i<n; ++i){
            weightedQuickUnionUF.union(i, TOP);
            weightedQuickUnionUF2.union(i, TOP);
        }
    }

    // opens the site (row, col) if it is not open already
    public void open(int row, int col){
        if (isOpen(row, col)){
            return;
        }

        int x=row-1;
        int y=col-1;
        int index=x*n+y;

        if (row == n){
            weightedQuickUnionUF.union(index, BOTTOM);
        }

        systems[x][y] = true;
        numOpenSites += 1;

        int[] dirctions = {-1, 0, 1, 0, -1};
        for (int i=0; i<dirctions.length-1; ++i) {
            int nx=x+dirctions[i];
            int ny=y+dirctions[i+1];
            if (nx>=0 && ny>=0 && nx<n && ny<n && isOpen(nx+1, ny+1)) {
                weightedQuickUnionUF.union(nx*n+ny, index);
                weightedQuickUnionUF2.union(nx*n+ny, index);
            }
        }
    }

    // is the site (row, col) open?
    public boolean isOpen(int row, int col) {
        if (row<=0 || col<=0 || row>n || col>n) {
            throw new IllegalArgumentException(String.format("row: %s, col: %s", row, col));
        }

        return systems[row-1][col-1];
    }

    // is the site (row, col) full?
    public boolean isFull(int row, int col) {
        int x=row-1;
        int y=col-1;

        return isOpen(row, col) && weightedQuickUnionUF2.find(n*x+y) == weightedQuickUnionUF2.find(TOP);
    }

    // returns the number of open sites
    public int numberOfOpenSites() {
        return numOpenSites;
    }

    // does the system percolate?
    public boolean percolates() {
        return weightedQuickUnionUF.find(TOP) == weightedQuickUnionUF.find(BOTTOM);
    }


    // test client (optional)
    public static void main(String[] args) {}

}
