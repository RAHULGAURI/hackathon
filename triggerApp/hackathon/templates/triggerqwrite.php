<?php

echo "Hello";
if(isset($_POST['Submit Query'])){//to run PHP script on submit
$myfile = fopen("trigger.txt", "w") or die("Unable to open file!");
	if(!empty($_POST['name'])){
		echo htmlspecialchars($_POST['name']);
	}	
fclose($myfile);
}

?>