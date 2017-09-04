<?php  

include_once BASE_PATH."/apibody/tools/index.php";

echo "world".PHP_EOL;

$slack = new SlackManager;
$slack->slackBot();

?>