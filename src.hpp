#pragma once

#include <bits/stdc++.h>

using namespace std;

int query(int x, int y, int z);

namespace {

struct TripleKey {
    int a, b, c;
    bool operator<(const TripleKey &other) const {
        if (a != other.a) return a < other.a;
        if (b != other.b) return b < other.b;
        return c < other.c;
    }
};

__int128 abs128(__int128 x) { return x < 0 ? -x : x; }

__int128 gcd128(__int128 a, __int128 b) {
    a = abs128(a);
    b = abs128(b);
    while (b != 0) {
        __int128 t = a % b;
        a = b;
        b = t;
    }
    return a;
}

struct Fraction {
    __int128 num = 0;
    __int128 den = 1;

    Fraction() = default;
    Fraction(__int128 n) : num(n), den(1) {}
    Fraction(__int128 n, __int128 d) : num(n), den(d) { normalize(); }

    void normalize() {
        if (den < 0) {
            den = -den;
            num = -num;
        }
        if (num == 0) {
            den = 1;
            return;
        }
        __int128 g = gcd128(num, den);
        num /= g;
        den /= g;
    }

    bool isZero() const { return num == 0; }
};

Fraction operator+(const Fraction &a, const Fraction &b) {
    Fraction r(a.num * b.den + b.num * a.den, a.den * b.den);
    r.normalize();
    return r;
}

Fraction operator-(const Fraction &a, const Fraction &b) {
    Fraction r(a.num * b.den - b.num * a.den, a.den * b.den);
    r.normalize();
    return r;
}

Fraction operator*(const Fraction &a, const Fraction &b) {
    Fraction r(a.num * b.num, a.den * b.den);
    r.normalize();
    return r;
}

Fraction operator/(const Fraction &a, const Fraction &b) {
    Fraction r(a.num * b.den, a.den * b.num);
    r.normalize();
    return r;
}

long long ask(int x, int y, int z) {
    return query(x, y, z);
}

long long evalTriple(const vector<long long> &values, int i, int j, int k) {
    long long mn = min({values[i], values[j], values[k]});
    long long mx = max({values[i], values[j], values[k]});
    return mn + mx;
}

bool solveFive(const array<long long, 10> &res, array<long long, 5> &sortedValues, array<int, 5> &perm) {
    const array<array<int, 3>, 10> triples = {{{0,1,2},{0,1,3},{0,1,4},{0,2,3},{0,2,4},{0,3,4},{1,2,3},{1,2,4},{1,3,4},{2,3,4}}};

    array<int, 5> order = {0, 1, 2, 3, 4};
    do {
        array<int, 5> rankOfPos{};
        for (int rank = 0; rank < 5; ++rank) rankOfPos[order[rank]] = rank;

        vector<vector<Fraction>> mat(10, vector<Fraction>(6));
        for (int t = 0; t < 10; ++t) {
            int a = triples[t][0], b = triples[t][1], c = triples[t][2];
            array<int, 3> ranks = {rankOfPos[a], rankOfPos[b], rankOfPos[c]};
            sort(ranks.begin(), ranks.end());
            mat[t][ranks[0]] = Fraction(1);
            mat[t][ranks[2]] = Fraction(1);
            mat[t][5] = Fraction(res[t]);
        }

        vector<int> pivotRow(5, -1);
        int row = 0;
        for (int col = 0; col < 5 && row < 10; ++col) {
            int pivot = -1;
            for (int i = row; i < 10; ++i) {
                if (!mat[i][col].isZero()) {
                    pivot = i;
                    break;
                }
            }
            if (pivot == -1) continue;
            swap(mat[pivot], mat[row]);
            Fraction inv = Fraction(1) / mat[row][col];
            for (int j = col; j <= 5; ++j) mat[row][j] = mat[row][j] * inv;
            for (int i = 0; i < 10; ++i) {
                if (i == row || mat[i][col].isZero()) continue;
                Fraction factor = mat[i][col];
                for (int j = col; j <= 5; ++j) mat[i][j] = mat[i][j] - factor * mat[row][j];
            }
            pivotRow[col] = row;
            ++row;
        }

        if (row != 5) continue;

        bool ok = true;
        array<long long, 5> values{};
        for (int col = 0; col < 5; ++col) {
            const Fraction &value = mat[pivotRow[col]][5];
            if (value.den != 1) {
                ok = false;
                break;
            }
            values[col] = (long long)value.num;
        }
        for (int i = 1; i < 5 && ok; ++i) {
            if (!(values[i - 1] < values[i])) ok = false;
        }
        array<long long, 5> assigned{};
        for (int rank = 0; rank < 5; ++rank) assigned[order[rank]] = values[rank];
        for (int t = 0; t < 10 && ok; ++t) {
            int a = triples[t][0], b = triples[t][1], c = triples[t][2];
            if (evalTriple(vector<long long>(assigned.begin(), assigned.end()), a, b, c) != res[t]) ok = false;
        }
        if (ok) {
            sortedValues = values;
            for (int i = 0; i < 5; ++i) perm[i] = order[i];
            return true;
        }
    } while (next_permutation(order.begin(), order.end()));
    return false;
}

long long computeAnswer(const vector<long long> &values) {
    const long long MOD = 998244353LL;
    long long pow233 = 233 % MOD;
    long long result = 0;
    for (size_t i = 0; i < values.size(); ++i) {
        result = (result + (__int128)(values[i] % MOD) * pow233) % MOD;
        pow233 = (__int128)pow233 * 233 % MOD;
    }
    return result;
}

} // namespace

int guess(int n, int Taskid) {
    (void)Taskid;
    vector<long long> ans(n + 1, -1);
    array<long long, 10> firstFive{};
    int idx = 0;
    for (int i = 1; i <= 5; ++i) {
        for (int j = i + 1; j <= 5; ++j) {
            for (int k = j + 1; k <= 5; ++k) {
                firstFive[idx++] = ask(i, j, k);
            }
        }
    }

    array<long long, 5> sortedValues{};
    array<int, 5> perm{};
    bool ok = solveFive(firstFive, sortedValues, perm);
    if (!ok) {
        return 0;
    }

    for (int t = 0; t < 5; ++t) ans[perm[t] + 1] = sortedValues[t];

    array<int, 5> sortedPos{};
    for (int i = 0; i < 5; ++i) sortedPos[i] = perm[i] + 1;

    for (int i = 6; i <= n; ++i) {
        int a = sortedPos[0];
        int b = sortedPos[2];
        int c = sortedPos[4];
        long long av = sortedValues[0];
        long long bv = sortedValues[2];
        long long cv = sortedValues[4];
        long long q1 = ask(a, b, i);
        long long q2 = ask(b, c, i);
        long long x;
        if (q1 == av + bv) {
            x = q2 - cv;
        } else if (q2 == bv + cv) {
            x = q1 - av;
        } else if (q1 < av + bv) {
            x = q1 - bv;
        } else {
            x = q2 - bv;
        }
        ans[i] = x;
    }

    vector<long long> values(n);
    for (int i = 1; i <= n; ++i) values[i - 1] = ans[i];
    return (int)computeAnswer(values);
}
