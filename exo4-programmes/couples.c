#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int even(int x){
	return (!(x % 2));
}

int max(int a, int b){
	if(a > b) 
		return a;
	return b;
}

long int orderedPairToCodeNat(int x, int y){
	long code;
	int sumxy; 
	sumxy = x + y;
	code = ((sumxy)*(sumxy + 1))/2;

	if(even(sumxy)){
		code = code + x;
	}
	else{
		code = code + y; 
	}
	return code;
}

int* codeToOrderedPairNat(long int code){
	int *couple = malloc(2*sizeof(int));
	int n = sqrt(code * 2);
	long int axis = (n * (n + 1))/2;
	int diff = 0;
	if(axis > code){
			n = n - 1;
			axis = (n * (n + 1))/2;
	}
	diff = code - axis;
	if(even(n)){
		couple[0] = diff;
		couple[1] = n - diff;
	}
	else{
		couple[0] = n - diff;
		couple[1] = diff;
	}
	return couple;
}

long int orderedPairToCodeInt(int x, int y){
	long int code = 0;	

	if (x < 0){
		x = (2 * (abs (x))) - 1;
	}
	else{
		x = 2 * x;
	}

	if (y < 0){
		y = (2 * (abs(y))) - 1;
	}
	else{
		y = 2 * y;
	}

	code = orderedPairToCodeNat(x, y);
	return code;
}

int* codeToOrderedPairInt(long int code){
	int *pair = codeToOrderedPairNat(code);

	if (even(pair[0])){
		pair[0] = pair[0] / 2;
	}
	else{
		pair[0] = (pair[0] / 2)*(-1) - 1;
	} 
	
	if (even (pair[1])){
		pair[1] = pair[1] / 2;
	}
	else{
		pair[1] = (pair[1] / 2)*(-1) - 1;
	}

	return pair;
}

long int orderedPairToCodeIntAlgo2(int x, int y){
	long int code = 0;
	int _x, _y, diff;
	_x = _y = diff = 0;
	int temp;
	int absmax;	
	absmax = max(abs(x), abs(y));	
	
	if(absmax == abs(x)){ // _x == x 
		_x = _y = x;
		temp = abs(x) * 2;
		if(x < 0){ // x negative
			code = temp * (temp + 1);
			if(y < 0){ // x negative, y negative
				diff = abs(_y) - abs(y);
			}
			else{ // x negative, y positive
				diff = abs(_y) + abs(y);
			}
		}
		else{ // x positive
			code = (temp - 1) * temp; 
			if(y > 0){ // x positive, y positive
				diff = abs(_y) - abs(y);
			}
			else{ // x positive, y negative
				diff = abs(_y) + abs(y);
			}
		}
		code = code - diff;
	}
	else{ // _y = y
		_x = _y = y;
		temp = abs(y) * 2;
		if(y < 0){ // y negative
			code = temp * (temp + 1);		
			if(x < 0){ // y negative, x negative
				diff = abs(_x) - abs(x);
			}
			else{ // y negative, x positive
				diff = abs(_x) + abs (x);
			}
		}
		else{ // y positive
			code = (temp - 1) * temp;
			if(x > 0){ // y positive, x positive
				diff = abs(_x) - abs(x);
			}
			else{ // y positive, x negative
				diff = abs(_x) + abs(x);
			}	
		}	
		code = code + diff;	
	}
	return code;
}

int* codeToOrderedPairIntAlgo2(long int code){
	int* pair = malloc(2*sizeof(int));
	int isqrtcode = (int) sqrt(code);
	long int pivotcode = isqrtcode * (isqrtcode + 1);
	int x, y;
	x = y = 0;	

	if(even(isqrtcode)){
		x = y = -(isqrtcode / 2);
		if(code > pivotcode){
			x = x + (code - pivotcode);
		}
		else{
			y = y + (pivotcode - code);
		}
	}
	else{
		x = y = (isqrtcode / 2) + 1;
		if(code > pivotcode){
			x = x - (code - pivotcode);
		}
		else{
			y = y - (pivotcode - code); 
		}
	}
	pair[0] = x;
	pair[1] = y; 
	return pair;
}

long int orderedMultipleToCodeNat(int *arr, int size){
	long int code;	
	if(size > 1){
		code = orderedPairToCodeNat(arr[size - 2], arr[size - 1]);
		arr[size - 2] = code;
		arr = realloc(arr, (size - 1));
		if(size > 2){		
			code = orderedMultipleToCodeNat(&arr[0], (size - 1));
		}
	}
	return code;
}

int* codeToOrderedMultipleNat(long int code, int size){
	int *multiple = malloc(size*sizeof(int));
	int *pair;
	int i = 0;
	for(i = 0; i < (size - 1); i++){
		pair = codeToOrderedPairNat(code);
		code = pair[1];
		multiple[i] = pair[0];
		multiple[i + 1] = pair[1];
	}
	return multiple;
}


long int orderedMultipleToCodeInt(int *arr, int size){
	long int code;	
	if(size > 1){
		code = orderedPairToCodeInt(arr[size - 2], arr[size - 1]);
		arr[size - 2] = code;
		arr = realloc(arr, (size - 1));
		if(size > 2){		
			code = orderedMultipleToCodeInt(&arr[0], (size - 1));
		}
	}
	return code;
}

int* codeToOrderedMultipleInt(long int code, int size){
	int *multiple = malloc(size*sizeof(int));
	int *pair;
	int i = 0;
	for(i = 0; i < (size - 1); i++){
		pair = codeToOrderedPairInt(code);
		code = pair[1];
		multiple[i] = pair[0];
		multiple[i + 1] = pair[1];
	}
	return multiple;
}


long int orderedMultipleToCodeIntAlgo2(int *arr, int size){
	long int code = 0;	
	if(size > 1){
		code = orderedPairToCodeIntAlgo2(arr[size - 2], arr[size - 1]);
		arr[size - 2] = code;
		arr = realloc(arr, (size - 1));
		if(size > 2){		
			code = orderedMultipleToCodeIntAlgo2(&arr[0], (size - 1));
		}
	}
	return code;
}

int* codeToOrderedMultipleIntAlgo2(long int code, int size){
	int *multiple = malloc(size*sizeof(int));
	int *pair;
	int i = 0;
	for(i = 0; i < (size - 1); i++){
		pair = codeToOrderedPairIntAlgo2(code);
		code = pair[1];
		multiple[i] = pair[0];
		multiple[i + 1] = pair[1];
	}
	return multiple;
}




void testMultiNat(){
	
	int x = 0;
	int y = 0;
	long int code = 0;
	int *p;
	int size = 0;

	do{		
		printf("\n(NATURAL NUMBERS) testPairInt();Enter a size of a multidimensional array representing a 'ordered multiple': ");
		scanf("%d",&size);
		p = malloc(size * sizeof(int));
		int i;
		for(i = 0; i < size; i++){
			printf("Enter value number %d: ", i);
			scanf("%d", &p[i]);
		}

		code = orderedMultipleToCodeNat(p, size);
		printf("\n... The code is %ld", code);
		printf ("\ncode = ");
		scanf("%ld",&code);
		printf ("\nnumber of ordered elements = ");
		scanf("%d",&size);
		p = codeToOrderedMultipleNat(code, size);
		printf("The ordered multiple identified by code %ld contains the following elements: ", code);
		printf("(");
	
		for(i = 0; i < (size - 1); i++){
			printf("%d, ", p[i]);
		}
		printf("%d)", p[size-1]);
	}
	while(1);
}

void testPairInt(){
	
	int x = 0;
	int y = 0;
	long int code = 0;
	int *p;

	do{		
		p = malloc(2 * sizeof(int));
		int i;
		for(i = 0; i < 2; i++){
			printf("(ORDERED PAIRS INT) Enter value number %d: ", i);
			scanf("%d", &p[i]);
		}

		code = orderedPairToCodeInt(p[0], p[1]);
		printf("\n... The code is %ld", code);
		printf ("\ncode = ");
		scanf("%ld",&code);
		p = codeToOrderedPairInt(code);
		printf("The ordered pair identified by code %ld contains the following elements: ", code);
		printf("(");
	
		for(i = 0; i < 1; i++){
			printf("%d, ", p[i]);
		}
		printf("%d)\n", p[1]);
	}
	while(1);
}

void testMultiInt(){
	
	int x = 0;
	int y = 0;
	long int code = 0;
	int *p;
	int size = 0;

	do{		
		printf("\n(INTEGERS) Enter a size of a multidimensional array representing a 'ordered multiple': ");
		scanf("%d",&size);
		p = malloc(size * sizeof(int));
		int i;
		for(i = 0; i < size; i++){
			printf("Enter value number %d: ", i);
			scanf("%d", &p[i]);
		}

		code = orderedMultipleToCodeInt(p, size);
		printf("\n... The code is %ld", code);
		printf ("\ncode = ");
		scanf("%ld",&code);
		printf ("\nnumber of ordered elements = ");
		scanf("%d",&size);
		p = codeToOrderedMultipleInt(code, size);
		printf("The ordered multiple identified by code %ld contains the following elements: ", code);
		printf("(");
	
		for(i = 0; i < (size - 1); i++){
			printf("%d, ", p[i]);
		}
		printf("%d)", p[size-1]);
	}
	while(1);
}

void testPairIntAlgo2(){
	
	int x = 0;
	int y = 0;
	long int code = 0;
	int *p;

	do{		
		p = malloc(2 * sizeof(int));
		int i;
		
		for(i = 0; i < 2; i++){
			printf("(ORDERED PAIRS INT) Enter value number %d: ", i);
			scanf("%d", &p[i]);
		}

		code = orderedPairToCodeIntAlgo2(p[0], p[1]);
		printf("\n... The code is %ld", code);	

		printf ("\ncode = ");
		scanf("%ld",&code);
		p = codeToOrderedPairIntAlgo2(code);
		printf("The ordered pair identified by code %ld contains the following elements: ", code);
		printf("(");
	
		for(i = 0; i < 1; i++){
			printf("%d, ", p[i]);
		}
		printf("%d)\n", p[1]);
	}
	while(1);
}


void testMultiIntAlgo2(){
	
	int x = 0;
	int y = 0;
	long int code = 0;
	int *p;
	int size = 0;

	do{		
		printf("\n(INTEGERS) Enter a size of a multidimensional array representing a 'ordered multiple': ");
		scanf("%d",&size);
		p = malloc(size * sizeof(int));
		int i;
		for(i = 0; i < size; i++){
			printf("Enter value number %d: ", i);
			scanf("%d", &p[i]);
		}

		code = orderedMultipleToCodeIntAlgo2(p, size);
		printf("\n... The code is %ld", code);
		printf ("\ncode = ");
		scanf("%ld",&code);
		printf ("\nnumber of ordered elements = ");
		scanf("%d",&size);
		p = codeToOrderedMultipleIntAlgo2(code, size);
		printf("The ordered multiple identified by code %ld contains the following elements: ", code);
		printf("(");
	
		for(i = 0; i < (size - 1); i++){
			printf("%d, ", p[i]);
		}
		printf("%d)", p[size-1]);
	}
	while(1);
}




int main(int argc, char **argv, char **envp){
	// testMultiNat();
	// testPairInt();
	// testMultiInt();
	// testPairIntAlgo2();
	 testMultiIntAlgo2();
}

