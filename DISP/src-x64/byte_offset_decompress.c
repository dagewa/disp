/* C code implementing the "byte_offset" decompression algorithm for miniCBF images */
/* this function is called by readCBF using the .C interface. */
/* NB all muti-byte integers in a mini CBF are stored as little endian, regardless of the byte order of the computer that wrote the file */

#include	<stdint.h>
#include	<limits.h>
#include	<stdio.h>

void decompress(char* binaryRaw, int* binarySize, int* pixelArray, int* errCode)
{
	int i = 0, j = 0, k, base = 0, delta;
	unsigned char bytes[8];
	int16_t  delta16;
	int64_t   delta64, base64;
	uint64_t  udelta64;

	*errCode = 0;

	while (i < *binarySize){

		delta = (int) binaryRaw[i];
		++i;
		if (delta != -128){
			base += delta;
			pixelArray[j] = base;
			++j;
			continue;
		}

		/* if we got here, the previous byte was equal to the multi-byte flag 0x80
		   so read the next two bytes. */

		for (k = 0; k < 2; k++, i++){
			bytes[k] = binaryRaw[i];
		}

		/* Need to cast to a 16 bit int first, otherwise the first two bytes of the
		   32 bit int will be all 0, whereas for negative delta in 2's complement
		   the first two bytes should be all 1. */
		delta16 = (int16_t) bytes[0] | (bytes[1] << 8);
		delta = (int) delta16;

		if (delta != -32768){
			base += delta;
			pixelArray[j] = base;
			++j;
			continue;
		}

		/* if we got here, the previous two bytes were equal to the multi-byte flag 0x8000
		   so read the next four bytes*/
		for (k = 0; k < 4; k++, i++)
			bytes[k] = binaryRaw[i];

		delta = (int) bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24);
		if (delta != -2147483648){
			base += delta;
			pixelArray[j] = base;
			++j;
			continue;
		}
		/* if we got here, the previous four bytes were equal to the multi-byte flag 0x80000000
		   so read the next eight bytes. Need to use a 64-bit int for this*/

		for (k = 0; k < 4; k++, i++)
			bytes[k] = binaryRaw[i];


		udelta64 = (uint64_t) bytes[0] |
				  ((uint64_t) bytes[1] <<  8) |
				  ((uint64_t) bytes[2] << 16) |
				  ((uint64_t) bytes[3] << 24) |
				  ((uint64_t) bytes[4] << 32) |
				  ((uint64_t) bytes[5] << 40) |
				  ((uint64_t) bytes[6] << 48) |
				  ((uint64_t) bytes[7] << 56);
		delta64 = (int64_t) udelta64;

		base64 = (int64_t) base;
		base64 += delta64;

		/* Now see if base64 can be represented by the type 'int' for output into pixelArray*/
		if (base64 > INT_MAX){
			*errCode = 1;
			break;
		} else {
			base = (int) base64;
			pixelArray[j] = base;
			++j;
		}
	}
}
