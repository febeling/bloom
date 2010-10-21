package com.github.febeling;

import static net.partow.GeneralHashFunctionLibrary.*;

public class NumericHelpers
{
    /** Mask for positive bits of a long, i.e. all except highest.
     */
    static final long MASK = 0x7fffffffffffffffL;

    public static final long[] indexes(String s, int m, int k) {
	long[] indexes = new long[k];
	indexes[0] = (APHash(s) & MASK) % m;   if(k == 1) return indexes;
	indexes[1] = (BKDRHash(s) & MASK) % m; if(k == 2) return indexes;
	indexes[2] = (BPHash(s) & MASK) % m;   if(k == 3) return indexes;
	indexes[3] = (DEKHash(s) & MASK) % m;  if(k == 4) return indexes;
	indexes[4] = (DJBHash(s) & MASK) % m;  if(k == 5) return indexes;
	// ELFHash
	// JSHash
	// PJWHash
	// RSHash
	// SDBMHash
	// FNVHash
	throw new RuntimeException("k > 5 not supported");
    }

    public static final byte[] BIT_MASKS = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, -0x80};

    public static boolean isBitSet(byte[] bytes, long i) {
	int b = bytes.length -1 - (int)(i / 8);
	byte bitmask = BIT_MASKS[(int)(i % 8)];
	return (bytes[b] & bitmask) == bitmask;
    }

    public static boolean contains(byte[] bytes, String s, int m, int k) {
	long[] indexes = indexes(s, m, k);

	for(int i = 0; i < indexes.length; i++) {
	    if(!isBitSet(bytes, indexes[i])) return false;
	}

	return true;
    }
}
