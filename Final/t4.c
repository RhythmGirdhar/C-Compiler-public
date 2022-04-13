#include<stdio.h>
#include<stdlib.h>

//ternary icg
//function call icg

void add(int a, int b){
	a = 12;
	b = 15;
}

int main(){
	int a = 3;
	int b = 4;

	int c = (a==b)? 10 : 20;

	int d, e, f, g;
	add(d, e, f, g);
}