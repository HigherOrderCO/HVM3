import struct, sys, zlib, os

fn = sys.argv[1] if len(sys.argv)>1 else 'mem-heat.png'
if not os.path.exists(fn):
    print('PNG missing')
    sys.exit(2)
with open(fn,'rb') as f:
    data=f.read()
if data[:8] != b'\x89PNG\r\n\x1a\n':
    print('Not a PNG')
    sys.exit(2)

off = 8
idat = b''
w = h = None
while off < len(data):
    ln = int.from_bytes(data[off:off+4], 'big'); off += 4
    typ = data[off:off+4]; off += 4
    chunk = data[off:off+ln]; off += ln
    off += 4  # skip CRC
    if typ == b'IHDR':
        w = int.from_bytes(chunk[0:4],'big')
        h = int.from_bytes(chunk[4:8],'big')
    elif typ == b'IDAT':
        idat += chunk
    elif typ == b'IEND':
        break

raw = zlib.decompress(idat)
all_white = True
nonwhite_total = 0
samples = []
for y in range(h):
    row = raw[y*(1+3*w):(y+1)*(1+3*w)]
    if row[0] != 0:
        print('Unexpected filter', row[0])
        sys.exit(2)
    pix = row[1:]
    for x in range(w):
        i = 3*x
        r,g,b = pix[i], pix[i+1], pix[i+2]
        if not (r == 255 and g == 255 and b == 255):
            all_white = False
            nonwhite_total += 1
            if len(samples) < 16:
                samples.append((x, y, int(r), int(g), int(b)))

print('all_white:', all_white)
print('nonwhite_total:', nonwhite_total)
print('samples:', samples)
sys.exit(0 if nonwhite_total > 0 else 1)
