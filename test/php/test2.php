<?php  
include_once BASE_PATH."/apibody/tools/index.php";

use Phalcon\Queue\Beanstalk;

$tube = "testTube_2";
while (1) {
    beanstalk_get($tube);
    select();
}

function select(){
    $mysql = new Mysql_Manager;
    $phql = "select * from users";
    $parameters = array(  
    );
    $r = $mysql->fetchAll($phql, $parameters);
    var_dump($r);
}

function beanstalk_get($tube){
    $queue = new Beanstalk(
        [
            "host" => "localhost",
            "port" => "11300",
            'persistent' => true,
        ]
    );

    $queue->choose($tube);
    $queue->watch($tube);

    $job = $queue->reserve(); // 獲取並鎖住任務
    // var_dump($job);
    $job->delete();
    $queue->quit();
}

?>