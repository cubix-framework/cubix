function main(){
	try { 
  		dadalert("Welcome Fellow Slytherin!"); 
		} 
	catch(err) { 
  		console.log(err); 
		}
	finally{
		alert( 'finally' );
		}
	try {
    		throw new Error('Yeah... Sorry');
	}
	catch(e) {
    		console.log(e);
	}
}
