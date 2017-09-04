<?php  
include_once BASE_PATH."/apibody/tools/index.php";
use Phalcon\Db\Adapter\Pdo\Mysql;
use Phalcon\Queue\Beanstalk;

$tube = "testTube_1";
while (1) {
    beanstalk_get($tube);

    $connection = db_connect();
    $phql = "select * from users";
    $result = $connection->fetchAll(
        $phql,
        \Phalcon\Db::FETCH_ASSOC
    );
    var_dump($result);
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
    $job->delete();
    $queue->quit();
}

function db_connect(){
    $dbconfig = config()->database;
    $connection = new Mysql(
        array(
            "host"      => $dbconfig->host,
            "username"  => $dbconfig->username,
            "password"  => $dbconfig->password,
            "dbname"    => $dbconfig->dbname,
            "port"      => $dbconfig->port
        )
    );
    $connection->connect();
    return $connection;
}

function config(){
    $config = include BASE_PATH."/app/config/config.php";
    return $config;
}

?>