import sys

W = H = 200

# Boundary between the two scene bands (see gscene/scene.ml).
# OCaml y < 99 (the EXACT zone) maps to screen rows >= 101; everything at
# screen row <= 100 is the AA zone (curved/stroked primitives, tolerated).
EXACT_FIRST_ROW = 101


def read_ppm(path):
    with open(path, "rb") as f:
        data = f.read()
    # parse P6 header: "P6\n200 200\n255\n"
    parts = []
    i = 2
    while len(parts) < 3:
        while data[i] in b" \t\n\r":
            i += 1
        j = i
        while data[j] not in b" \t\n\r":
            j += 1
        parts.append(data[i:j]); i = j
    i += 1  # single whitespace after maxval
    body = data[i:]
    return [(body[3 * k], body[3 * k + 1], body[3 * k + 2]) for k in range(W * H)]


def read_hex(path):
    with open(path) as f:
        s = f.read().strip()
    px = []
    for k in range(W * H):
        o = k * 6
        px.append((int(s[o:o + 2], 16), int(s[o + 2:o + 4], 16), int(s[o + 4:o + 6], 16)))
    return px


native = read_ppm(sys.argv[1])
web = read_hex(sys.argv[2])

exact_diff = 0          # diffs in the exact zone -> must be 0
aa_diff = 0             # diffs in the AA zone -> tolerated
aa_near = 0             # of those, AA fringe (<=32 per channel)
examples = []           # exact-zone mismatches (the ones that matter)

for k in range(W * H):
    c = k % W
    r = k // W
    n = native[k]
    we = web[k]
    if n == we:
        continue
    if r >= EXACT_FIRST_ROW:
        exact_diff += 1
        if len(examples) < 16:
            examples.append((c, r, n, we))
    else:
        aa_diff += 1
        d = max(abs(n[0] - we[0]), abs(n[1] - we[1]), abs(n[2] - we[2]))
        if d <= 32:
            aa_near += 1

print(f"image: {W}x{H} = {W * H} px")
print(f"EXACT zone (rows >= {EXACT_FIRST_ROW}) diffs : {exact_diff}   (MUST be 0)")
print(f"AA zone    (rows <= {EXACT_FIRST_ROW - 1}) diffs : {aa_diff}   "
      f"({aa_near} near-match <=32/chan, rest hard AA edges)")
if examples:
    print("EXACT-zone mismatches (col,row  native vs web):")
    for c, r, n, we in examples:
        print(f"   ({c:3d},{r:3d}) {n} vs {we}")
if exact_diff == 0:
    print("RESULT: PASS  (every solid fill / poly / plot / image matches native)")
else:
    print(f"RESULT: FAIL  ({exact_diff} exact-zone pixels differ from native)")
sys.exit(1 if exact_diff else 0)
