int main() {
    __label__ x, z;
    goto y;
    int bar() {
        goto y;
        y: ;
        goto x;
        
        int baz() {
            __label__ x;
            goto x;
	    x: ;
            goto z;
	    y: ;
            goto y;
        }
    }
    
    x: ;
    y: ;
    z: ;
}
