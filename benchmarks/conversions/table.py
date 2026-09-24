import sys, collections
# table.py RESULTS — per program and --opt level, counts with LCM on / off
rows = collections.defaultdict(dict)
for l in open(sys.argv[1]):
    f = l.split()
    if len(f) < 3: continue
    n, o, m = f[:3]
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
print(f"{'program':16} {'opt':>3} | " + " | ".join(f"{k + ' on/off':>17}" for k in kinds))
tot = collections.Counter()
for (n, o) in sorted(rows):
    a, b = rows[(n, o)].get('lcm', {}), rows[(n, o)].get('nolcm', {})
    if not any(a.values()) and not any(b.values()): continue
    for k in kinds:
        tot[(o, k, 'on')] += a.get(k, 0); tot[(o, k, 'off')] += b.get(k, 0)
    cell = lambda k: f"{fmt(a.get(k,0)):>8}/{fmt(b.get(k,0)):<8}"
    print(f"{n:16} {o:>3} | " + " | ".join(cell(k) for k in kinds))
for o in ('1', '2'):
    print(f"{'TOTAL':16} {o:>3} | " + " | ".join(f"{fmt(tot[(o,k,'on')]):>8}/{fmt(tot[(o,k,'off')]):<8}" for k in kinds))
