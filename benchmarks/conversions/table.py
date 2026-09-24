import sys, collections
# table.py RESULTS — per program and --opt level, the counts in each
# configuration, separated by slashes
rows = collections.defaultdict(dict)
modes = []
for l in open(sys.argv[1]):
    f = l.split()
    if len(f) < 3: continue
    n, o, m = f[:3]
    if m not in modes: modes.append(m)
    c = collections.Counter()
    for kv in f[3:]:
        k, v = kv.split('=')
        kind = 'box' if k.startswith('box') else 'unbox' if k.startswith('unbox') else k
        c[kind] += int(v)
    rows[(n, o)][m] = c
def fmt(x):
    for u, d in (('G', 1e9), ('M', 1e6), ('k', 1e3)):
        if x >= d: return f"{x/d:.3g}{u}"
    return str(x)
# Large-integer conversions (portable integers) are only shown when present
kinds = ['box', 'unbox', 'tag', 'untag'] + [
    k for k in ('tag64', 'untag64')
    if any(c.get(k, 0) for r in rows.values() for c in r.values())]
width = 9 * len(modes) + 1
print(f"{'program':16} {'opt':>3} | " + " | ".join(f"{k:>{width}}" for k in kinds))
print(f"{'':16} {'':>3} | " + " | ".join(f"{'/'.join(modes):>{width}}" for k in kinds))
tot = collections.Counter()
for (n, o) in sorted(rows):
    r = rows[(n, o)]
    if not any(any(c.values()) for c in r.values()): continue
    for m in modes:
        for k in kinds:
            tot[(o, k, m)] += r.get(m, {}).get(k, 0)
    cell = lambda k: "/".join(f"{fmt(r.get(m, {}).get(k, 0)):>8}" for m in modes)
    print(f"{n:16} {o:>3} | " + " | ".join(f"{cell(k):>{width}}" for k in kinds))
for o in ('1', '2'):
    cell = lambda k: "/".join(f"{fmt(tot[(o, k, m)]):>8}" for m in modes)
    print(f"{'TOTAL':16} {o:>3} | " + " | ".join(f"{cell(k):>{width}}" for k in kinds))
