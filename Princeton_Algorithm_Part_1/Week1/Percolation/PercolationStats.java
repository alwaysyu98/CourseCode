/* *****************************************************************************
 *  Name:              Jiri Yu
 *  Last modified:     10/23/2021
 **************************************************************************** */

import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdStats;

public class PercolationStats {
    private double[] p;
    // perform independent trials on an n-by-n grid
    public PercolationStats(int n, int trials) {
        if (n<=0 || trials<=0){
            throw new IllegalArgumentException();
        }
        p = new double[trials];
        for (int i=0; i<trials; ++i){
            Percolation percolation = new Percolation(n);
            while (!percolation.percolates()) {
                int tmp = StdRandom.uniform(n*n);
                int row = tmp/n;
                int col = tmp%n;

                percolation.open(row+1, col+1);
            }
            p[i] = (double)percolation.numberOfOpenSites() / n / n;
        }
    }

    // sample mean of percolation threshold
    public double mean() {
        return StdStats.mean(p);
    }

    // sample standard deviation of percolation threshold
    public double stddev() {
        return StdStats.stddev(p);
    }

    // low endpoint of 95% confidence interval
    public double confidenceLo() {
        return mean()-1.96*stddev()/Math.sqrt(p.length);
    }

    // high endpoint of 95% confidence interval
    public double confidenceHi() {
        return mean()+1.96*stddev()/Math.sqrt(p.length);
    }

    // test client (see below)
    public static void main(String[] args) {
        PercolationStats percolationStats = new PercolationStats(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
        System.out.println(String.format("mean\t= %.16f", percolationStats.mean()));
        System.out.println(String.format("stddev\t= %.16f", percolationStats.stddev()));
        System.out.println(String.format("95%% confidence interval\t= [%.16f, %.16f]",
                                         percolationStats.confidenceLo(),
                                         percolationStats.confidenceHi()));
    }
}
