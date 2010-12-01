#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int pair(int x){
	return (!(x % 2));
}

/*
int code_couples_very_slow(int _x, int _y){
	int x, y;
	x = y = 0;
	int code = 0;
	char direction = 'r'; // 'l' pour "left-down", 'r' pour "right-up"
	printf("In the 'couples' function\n");
	sleep(1);
	while(((x != _x) || (y != _y))){
		if((y == 0) && (pair(x))){
			printf("IF1:\n");	
			sleep(1);		
			x++;
			code++;
			direction = 'l';
			printf("IF N° 1, x = %d, y = %d, code = %d\n", x, y, code);
			sleep(1);
		}
		else if((x == 0) && (!pair(y))){
			printf("IF2:\n");			
			sleep(1);
			y++;
			code++;
			direction = 'r';	
			printf("IF N° 2, x = %d, y = %d, code = %d\n", x, y, code);
			sleep(1);
		}
		else if((y == 0) && (direction == 'l')){
			printf("IF3:\n");
			sleep(1);			
			while((x != 0) && ((x != _x) || (y != _y))){
				printf("WHILE3:\n");
				sleep(1);
				x--;
				y++;
				code++;
				printf("IF N° 3, x = %d, y = %d, code = %d\n", x, y, code);
				sleep(1);
			}
		}
		else if((x == 0) && (direction == 'r')){
			printf("IF4:\n");			
			sleep(1);
			while((y != 0) && ((x != _x) || (y != _y))){
				printf("WHILE4:\n");
				sleep(1);
				x++;
				y--;
				code++;
				printf("IF N° 4, x = %d, y = %d, code = %d\n", x, y, code);
				sleep(1);
			}
		}
	} 
	return code;
}

int code_couples_slow(int _x, int _y){
	int x, y;
	x = y = 0;
	int code = 0;
	char direction = 'r'; // 'l' pour "left-down", 'r' pour "right-up"
	while(((x != _x) || (y != _y))){
		if((y == 0) && (pair(x))){
			x++;
			code++;
			direction = 'l';
		}
		else if((x == 0) && (!pair(y))){
			y++;
			code++;
			direction = 'r';	
		}
		else if((y == 0) && (direction == 'l')){
			while((x != 0) && ((x != _x) || (y != _y))){
				x--;
				y++;
				code++;
			}
		}
		else if((x == 0) && (direction == 'r')){
			while((y != 0) && ((x != _x) || (y != _y))){
				x++;
				y--;
				code++;
			}
		}
	} 
	return code;
}
*/

int orderedPairToCode(int x, int y){
	int sumxy, code;
	sumxy = x + y;
	code = ((sumxy)*(sumxy + 1))/2;

	if(pair(sumxy)){
		code = code + x;
	}
	else{
		code = code + y; 
	}
	return code;
}


int* codeToOrderedPair(int code){
	int *couple = malloc(2*sizeof(int));
	int n = sqrt(code * 2);
	int axis = (n * (n + 1))/2;
	int diff = 0;
	if(axis > code){
		while(axis > code){
			n = n - 1;
			axis = (n * (n + 1))/2;
		}
	}
	else if(axis < code){
		while(axis < code){
			n = n + 1;
			axis = (n * (n + 1))/2;
		}
		if(axis > code){
			n = n - 1;
			axis = (n * (n + 1))/2;
		}
	}

	if(axis == code){ // je pense que je peux me dispenser de cet "if", ça revient au même car diff serait = 0
		if(pair(n)){
			couple[0] = 0;
			couple[1] = n;
		}
		else{
			couple[0] = n;
			couple[1] = 0;
		}
	}
	if(axis != code){ // idem
		diff = code - axis;
		if(pair(n)){
			couple[0] = diff;
			couple[1] = n - diff;
		}
		else{
			couple[0] = n - diff;
			couple[1] = diff;
		}
	}
	return couple;
}

int* codeToOrderedPair2(int code){
	int *couple = malloc(2*sizeof(int));
	int n = sqrt(code * 2);
	int axis = (n * (n + 1))/2;
	int diff = 0;
	if(axis > code){
			n = n - 1;
			axis = (n * (n + 1))/2;
	}
	diff = code - axis;
	if(pair(n)){
		couple[0] = diff;
		couple[1] = n - diff;
	}
	else{
		couple[0] = n - diff;
		couple[1] = diff;
	}
	return couple;
}


int	orderedMultipleToCode(int *arr, int size){
	int code;	
	if(size > 1){
		code = orderedPairToCode(arr[size - 2], arr[size - 1]);
		arr[size - 2] = code;
		arr = realloc(arr, (size - 1));
		if(size > 2){		
			code = orderedMultipleToCode(&arr[0], (size - 1));
		}
	}
	return code;
}

int* codeToOrderedMultiple(int code, int size){
	int *multiple = malloc(size*sizeof(int));
	int *pair;
	int i = 0;
	for(i = 0; i < (size - 1); i++){
		pair = codeToOrderedPair(code);
		code = pair[1];
		multiple[i] = pair[0];
		multiple[i + 1] = pair[1];
	}
	return multiple;
}


int main(int argc, char **argv, char **envp){

	int x = 0;
	int y = 0;
	int code = 0;
	int *p;
	int size = 0;

	do{
	/*
		printf ("\nx = ");
		scanf("%d",&x);
		printf ("y = ");
		scanf("%d",&y);
		code = orderedPairToCode(x, y);
		printf("Le code du couple (%d, %d) est %d\n", x, y, code);

		printf ("\ncode = ");
		scanf("%d",&code);
		p = codeToOrderedPair(code);
		printf("The ordered pair identified by code %d is (%d, %d)", code, p[0], p[1]);
	
*/			
		printf("\nEnter a size of a multidimensional array representing a 'ordered multiple': ");
		scanf("%d",&size);
		p = malloc(size * sizeof(int));
		int i;
		for(i = 0; i < size; i++){
			printf("Enter value number %d: ", i);
			scanf("%d", &p[i]);
		}

		code = orderedMultipleToCode(p, size);
		printf("\n... The code is %d", code);
	


		printf ("\ncode = ");
		scanf("%d",&code);
		printf ("\nnumber of ordered elements = ");
		scanf("%d",&size);
		p = codeToOrderedMultiple(code, size);
		printf("The ordered multiple identified by code %d contains the following elements: ", code);
		printf("(");
		for(i = 0; i < (size - 1); i++){
			printf("%d, ", p[i]);
		}
		printf("%d)", p[size-1]);


	}
	while(code != -1);
}

