<?php  
// $time = new SYSTime;
// $topic = '/test/'.$time->getNowTimestamp();

// 取得參數
$GLOBALS['topic'] = "/max/topic/help/to/do/somthing/two";
$GLOBALS['task_uuid'] = "ssid_2";
$GLOBALS['loopbreak'] = false;
connMqtt();

function connMqtt(){
    $topic = $GLOBALS['topic'];
    $task_uuid = $GLOBALS['task_uuid'];

    $broker     = "";
    $port       = 1883;
    $username   = "";
    $password   = "";
    // $clientId   = $stationIp."_subRobot_".$task_uuid."_".getmypid();
    $Qos        = 2;

    $client = new Mosquitto\Client();
    $client->setCredentials($username, $password);
    $client->connect($broker, $port, 20);
    $client->onConnect('connect');
    $client->onDisconnect('disconnect');
    $client->onSubscribe('subscribe');
    $client->onMessage('message');
    $client->subscribe($topic, $Qos); // Subscribe to all messages

    $GLOBALS['mqttclient'] = $client;

    // while (true) {
    echo date("Y/m/d h:i:sa").PHP_EOL;
    $client->loopForever(3600);
    echo PHP_EOL;
    //     if ($GLOBALS['loopbreak']) {
    //         break;
    //     }
    // }
}

function connect($r) {
    echo "Received response code {$r}\n";
}

function subscribe() {
    echo "Subscribed to a topic\n";
}

function message($message) {
    echo PHP_EOL.PHP_EOL;
    echo "\n收到 MQTT 訊息\n";
    printf("Got a message on topic=>%s, with payload=>%s\n", $message->topic, $message->payload);


    var_dump(memory_get_usage());
}

function disconnect($r) {
    echo "Disconnected cleanly : ".$r.PHP_EOL;
    // 做一個重複連線的任務
    // 更新 db 狀態
    
    if ($r!=0) {
        echo "reConnect....\n";
        connMqtt();
    }
    else if($r==0){
        // 收到停止的指令
    }
}




?>