var mode
var inputError = false
var inputErrorMessage

document.getElementById('output').style.visibility = 'hidden' 
 
document.getElementById('input').addEventListener('input',function(e){ 
		let gpa = e.target.value;
		gpa = parseFloat(gpa)
		if (gpa <0 || gpa >12) {
			alert("The cumulative gpa you entered is not valid")
		}else { 
			update(gpa)  
		}
}) 

var update = function(gpa) {
	document.getElementById('output').style.visibility  = 'visible'
	var output1= document.getElementById('4output') 
   	var output2 = document.getElementById('aplhaOutput') 
   	var output3 = document.getElementById('rangeOutput') 

   	var scaleFour = convert(gpa)[0]
   	var alpha = convert(gpa)[1]
   	var range = convert(gpa)[2]

   	output1.innerHTML = scaleFour
   	output2.innerHTML = alpha
   	output3.innerHTML = range
}

var convert = function(gpa) { 
	var gpa = Math.floor(gpa)
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

var alphaDict = {
	"A+":12,
	"A":11,
	"A-":10,
	"B+":9,
	"B":8,
	"B-":7,
	"C+":6,
	"C":5,
	"C-":4,
	"D+":3,
	"D":2,
	"D-":1,
	"F":0 
}
parseMarks = function(mode,marks) {
	var markArr = marks.split(" ")
	
	if(mode === "Numeric") {
		var sum = 0	
		for(var i=0;i<markArr.length;i++ ) {
			if(isNaN(markArr[i]) || markArr[i] < 0 || markArr[i]>12)  {
				inputError = true
			}
			sum += parseFloat(markArr[i]) 
		}
		return sum/ markArr.length
	}
	else if(mode === "Alpha") {
		var sum = 0
		for(var i=0;i<markArr.length;i++) {
			var singleMark = alphaDict[markArr[i]] 
			if(isNaN(singleMark)) {
				inputError = true
			}
			sum += singleMark 
		}
		return sum/ markArr.length
	} 

}
let options = document.querySelectorAll('.dropdown-menu a')
options.forEach(function(option){
	option.addEventListener('click',function(e){
		e.preventDefault()
		var selText = this.textContent
		button = document.getElementById('modeButton')
		button.textContent = selText
		mode = selText
	})
}) 

var submit = document.getElementById('submitButton')
submit.addEventListener('click',function(e){
	if(mode !== null) {
		var submitText = document.getElementById('submitText')
	    var markStr = submitText.value
	    	if (markStr !==null) {
	    		var gpaNum = parseMarks(mode, markStr)
	    	if(mode === undefined) {
	    		alert('Please select the calculation mode first') 
	    		return
	    	}
			if (gpaNum == null) {
				inputError = true 
			}else { 
		   		if(inputError) {
		   			alert("Invalid input. Check help page for more info.")
		   			inputError = false
		   		}else {
		   			update(gpaNum)
	   		    } 
			} 
	    }
		
	}
})


