package net.partow;
/*
 **************************************************************************
 *                                                                        *
 *          General Purpose Hash Function Algorithms Library              *
 *                                                                        *
 * Author: Arash Partow - 2002                                            *
 * URL: http://www.partow.net                                             *
 * URL: http://www.partow.net/programming/hashfunctions/index.html        *
 *                                                                        *
 * Copyright notice:                                                      *
 * Free use of the General Purpose Hash Function Algorithms Library is    *
 * permitted under the guidelines and in accordance with the most current *
 * version of the Common Public License.                                  *
 * http://www.opensource.org/licenses/cpl1.0.php                          *
 *                                                                        *
 **************************************************************************
*/


public class GeneralHashFunctionLibrary
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
	// FNVHash
	// JSHash
	// PJWHash
	// RSHash
	// SDBMHash
	throw new RuntimeException("k > 5 not supported yet");
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

   public static long RSHash(String str)
   {
      int b     = 378551;
      int a     = 63689;
      long hash = 0;

      for(int i = 0; i < str.length(); i++)
      {
         hash = hash * a + str.charAt(i);
         a    = a * b;
      }

      return hash;
   }
   /* End Of RS Hash Function */


   public static long JSHash(String str)
   {
      long hash = 1315423911;

      for(int i = 0; i < str.length(); i++)
      {
         hash ^= ((hash << 5) + str.charAt(i) + (hash >> 2));
      }

      return hash;
   }
   /* End Of JS Hash Function */


   public static long PJWHash(String str)
   {
      long BitsInUnsignedInt = (long)(4 * 8);
      long ThreeQuarters     = (long)((BitsInUnsignedInt  * 3) / 4);
      long OneEighth         = (long)(BitsInUnsignedInt / 8);
      long HighBits          = (long)(0xFFFFFFFF) << (BitsInUnsignedInt - OneEighth);
      long hash              = 0;
      long test              = 0;

      for(int i = 0; i < str.length(); i++)
      {
         hash = (hash << OneEighth) + str.charAt(i);

         if((test = hash & HighBits)  != 0)
         {
            hash = (( hash ^ (test >> ThreeQuarters)) & (~HighBits));
         }
      }

      return hash;
   }
   /* End Of  P. J. Weinberger Hash Function */


   public static long ELFHash(String str)
   {
      long hash = 0;
      long x    = 0;

      for(int i = 0; i < str.length(); i++)
      {
         hash = (hash << 4) + str.charAt(i);

         if((x = hash & 0xF0000000L) != 0)
         {
            hash ^= (x >> 24);
         }
         hash &= ~x;
      }

      return hash;
   }
   /* End Of ELF Hash Function */


   public static long BKDRHash(String str)
   {
      long seed = 131; // 31 131 1313 13131 131313 etc..
      long hash = 0;

      for(int i = 0; i < str.length(); i++)
      {
         hash = (hash * seed) + str.charAt(i);
      }

      return hash;
   }
   /* End Of BKDR Hash Function */


   public static long SDBMHash(String str)
   {
      long hash = 0;

      for(int i = 0; i < str.length(); i++)
      {
         hash = str.charAt(i) + (hash << 6) + (hash << 16) - hash;
      }

      return hash;
   }
   /* End Of SDBM Hash Function */


   public static long DJBHash(String str)
   {
      long hash = 5381;

      for(int i = 0; i < str.length(); i++)
      {
         hash = ((hash << 5) + hash) + str.charAt(i);
      }

      return hash;
   }
   /* End Of DJB Hash Function */


   public static long DEKHash(String str)
   {
      long hash = str.length();

      for(int i = 0; i < str.length(); i++)
      {
         hash = ((hash << 5) ^ (hash >> 27)) ^ str.charAt(i);
      }

      return hash;
   }
   /* End Of DEK Hash Function */


   public static long BPHash(String str)
   {
      long hash = 0;

      for(int i = 0; i < str.length(); i++)
      {
         hash = hash << 7 ^ str.charAt(i);
      }

      return hash;
   }
   /* End Of BP Hash Function */


   public static long FNVHash(String str)
   {
      long fnv_prime = 0x811C9DC5;
      long hash = 0;

      for(int i = 0; i < str.length(); i++)
      {
      hash *= fnv_prime;
      hash ^= str.charAt(i);
      }

      return hash;
   }
   /* End Of FNV Hash Function */


   public static long APHash(String str)
   {
      long hash = 0xAAAAAAAA;

      for(int i = 0; i < str.length(); i++)
      {
         if ((i & 1) == 0)
         {
            hash ^= ((hash << 7) ^ str.charAt(i) * (hash >> 3));
         }
         else
         {
            hash ^= (~((hash << 11) + str.charAt(i) ^ (hash >> 5)));
         }
      }

      return hash;
   }
   /* End Of AP Hash Function */

}
