document.getElementById('output').style.visibility = 'hidden' 
 
document.getElementById('input').addEventListener('input',function(e){ 
		let gpa = e.target.value;
		console.log(gpa)
		document.getElementById('output').style.visibility = 'visible'

    	var output1= document.getElementById('4output') 
   	 	var output2 = document.getElementById('aplhaOutput') 
   	 	var output3 = document.getElementById('rangeOutput') 

   	 	var scaleFour = convert(gpa)[0]
   	 	var alpha = convert(gpa)[1]
   	 	var range = convert(gpa)[2]

   	 	if(scaleFour == null || alpha == null || range == null) {
   	 		// error
   	 	}else {
   	 		output1.innerHTML = scaleFour
    	    output2.innerHTML = alpha
    	    output3.innerHTML = range
   	 	}
   	 	
}) 

var convert = function(str) {
	gpa = Math.floor(parseFloat(str))

	// var scale4, alpha, percentage
	switch(gpa) {
		case 12:
		   return [4.0, "A+", "90-100"] 
		   break;
		case 11:
		   return [3.9, "A", "85-89"]
		   break;
		case 10:
		   return [3.7, "A-", "80-84"]
		   break;
		case 9:
		   return [3.3, "B+", "77-79"]
		   break;
		case 8: 
		   return [3.0, "B", "73-76"]
		   break;
		case 7:
		   return [2.7, "B-", "70-72"]
		   break;
		case 6:
		   return [2.3, "C+", "67-69"]
		   break;
		case 5:
		   return [2.0, "C", "63-66"]
		   break;
		case 4:
		   return [1.7, "C-", "60-62"]
		   break;
		case 3:
		   return [1.3, "D+", "57-59"]
		   break;
		case 2:
		   return [1.0, "D", "53-56"]
		   break;
		case 1:
		   return [0.7, "D-", "50-52"]
		   break;
		case 0:
		   return [0.0, "F", "0-49"]
		   break;
		default:
		   return null
	}
}