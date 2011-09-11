/* C code implementing the "byte_offset" compression algorithm for miniCBF images */
/* this function is called by writeCBF using the .C interface. */
/* NB all muti-byte integers in a mini CBF are stored as little endian, regardless of the byte order of the computer that wrote the file */

#include	<stdio.h>

void compress(int* imageArr, int* imageArrLen, unsigned char* binArr, int* binByteLenMax, int* binByteLen, int* errCode)
{
	int i, j = 0;
	int base = 0, delta;
	unsigned char* intToBytes; 

	/* set errCode*/
	*errCode = 0;
	
	/* loop through imageArr */
	for (i = 0; i < *imageArrLen; i++){

		/* Compute the difference delta between the current pixel value and the base pixel value. */
		delta = imageArr[i] - base;
		
		/* If -127 <= delta <= 127, output delta as one byte and make the current pixel value the base pixel value */
		if (delta <= 127 && delta >= -127){
		
			binArr[j] = (unsigned char)delta;
			++j;
		
		/* If -32767 =< delta =< 32767, output the overload code for the first byte, then 
		output delta as a little_endian 16-bit quantity, then make the current pixel value the base pixel value */
		} else if (delta <= 32767 && delta >= -32767){
			
			/* 1 byte overload code */
			binArr[j] = 0x80;
			++j;
			
			/* write delta as a little endian 2 byte integer */
			intToBytes = (unsigned char*)&delta;
			binArr[j] = intToBytes[0];
			++j;
			binArr[j] = intToBytes[1];
			++j;
		
		/* If -2147483647 =< delta =< 2147483647, output the overload code for the first byte, and the following two bytes. Then
		output delta as a little_endian 32 bit quantity, then make the current pixel value the base pixel value */
		} else if (delta <= 2147483647L && delta >= -2147483647L){
			
			/* 1 byte overload code */
			binArr[j] = 0x80;
			++j;

			/* 2 byte overload code */
			binArr[j] = 0x00;		
			++j;			
			binArr[j] = 0x80;
			++j;

			/* write delta as a little endian 4 byte integer */			
			intToBytes = (unsigned char*)&delta;
			binArr[j] = intToBytes[0];
			++j;			
			binArr[j] = intToBytes[1];		
			++j;		
			binArr[j] = intToBytes[2];
			++j;			
			binArr[j] = intToBytes[3];
			++j;			
			
		} else {

			/* if we got here, delta is not in the supported range. Exit with an error */
			*errCode = 1;
			break;			
		
		}
		
		
		/* check we still have room in binArr to add up to another 7 bytes */
		if (j > *binByteLenMax - 7){
		
			*errCode = 2;
			break;	
			
		}

		/* set base to the current pixel value for the next iteration*/
		base = imageArr[i];
	}
	
	/* clean up ready to return */
	*binByteLen = j;	
}

/* test code 
int main(void)
{
	int imageArr[10] = {1, 2, 1, 5, 300 ,3, 7, 65537, 9, 50};
	int imageArrLen = sizeof(imageArr)/sizeof(int);
	unsigned char binArr[40];
	int binByteLenMax = sizeof(binArr)/sizeof(char);
	int binByteLen = 0;
	int errCode;
	int i;
	
	printf("\nerrcode = %d\n",errCode);
	
	compress(imageArr, &imageArrLen, binArr, &binByteLenMax, &binByteLen, &errCode);
	
	printf("\nerrcode = %d\n",errCode);
	
	for (i = 0; i < binByteLen; i++) printf("%x\n", binArr[i]);
	
	return 0;
}
*/
