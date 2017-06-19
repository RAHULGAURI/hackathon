<?php

echo "Hello";
if(isset($_POST['Submit'])){//to run PHP script on submit
	$myfile = fopen("trigger.txt", "w") or die("Unable to open file!");
if(!empty($_POST['check_list'])){
 //Loop to store and display values of individual checked checkbox.
foreach($_POST['check_list'] as $selected)
{
echo $selected."</br>";
fwrite($myfile, $selected);
}
}
fclose($myfile);
}

?>