# -*- mode: snippet -*-
# name: my competetive programming template for C++
# uuid:
# key: temp.cpp
# condition: t
# --
#pragma GCC optimize("O2")
#include <bits/stdc++.h>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<int, int> pii;
typedef pair<ll, ll> pll;
#define all(x) (x).begin(), (x).end()
#define lc(x) (x) << 1
#define rc(x) (x) << 1 | 1
#define midi(x, y) ((x) + (y)) >> 1
#define F first
#define S second
#define fast_io                                                                \\
  ios::sync_with_stdio(false);                                                 \\
  cin.tie(0);                                                                  \\
  cout.tie(0);
#define file_io                                                                \\
  freopen("in.txt", "r+", stdin);                                              \\
  freopen("out.txt", "w+", stdout);
ll poww(ll a, ll b, ll md) {
  ll ans = 1;
  for (; b; b >>= 1, a = a * a % md) {
    if (b & 1) {
      ans = ans * a % md;
    }
  }
  return ans % md;
}

const int MAXN = 1e6 + 10;
const int MAXLG = 18;
const int MOD = 1e9 + 7;
const int MOD2 = 998244353;
const ll INF = 8e18;
$1
int main() {
  $0
}
