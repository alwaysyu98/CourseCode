#lang planet neil/sicp

; dp
; dp[n][m] = dp[n-coins[m]][m] + dp[n][m-1], m>=1
; dp[n][m] = 0, n!=0 and m=0
; dp[n][m] = 1, n=0

; do not use memoization
; growth of time is like Fib(n)
; growth of space is like Fib(n)