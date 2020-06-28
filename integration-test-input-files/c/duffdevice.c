int main(void){

	send(to, from, count);
	register short *to, *from;
	register count;
	register n = (count + 7) / 8;  // gives the number of iterations
    		 
	switch (count % 8) {
    		case 0: do { *to = *from++;
    		case 7:      *to = *from++;
    		case 6:      *to = *from++;
    		case 5:      *to = *from++;
    		case 4:      *to = *from++;
    		case 3:      *to = *from++;
    		case 2:      *to = *from++;
    		case 1:      *to = *from++;
            	 	} while (--n > 0);
    		}
}
