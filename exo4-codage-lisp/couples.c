#include <stdio.h>
#include <stdlib.h>

int pair(int x){
	return (!(x % 2));
}

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


int code_couples_faster(int _x, int _y){
	int x, y;
	x = y = 0;
	int code = 0;
	int incr_int = 0;
	char direction = 'r'; // 'l' pour "left-down", 'r' pour "right-up"
	while(((x != _x) || (y != _y))){
		if((y == 0) && (pair(x))){
			x++;
			code++;
			incr_int++;
			direction = 'l';
		}
		else if((x == 0) && (!pair(y))){
			y++;
			code++;
			incr_int++;
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




int main(int argc, char **argv, char **envp){
	printf("hello\n");
	int code = 0;	
	int x, y, z;
	x = 11;
	y = 5;
	//z = -2;
	code = code_couples_slow(x,y);
	printf("Le code du couple (%d, %d) est %d\n", x, y, code);
}
