int gcd(int n1, int n2) {
    return (n2 == 0) ? n1 : gcd(n2, n1 % n2);
}