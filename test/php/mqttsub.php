<?php  
$brokerInfo = array(
    'ip' => '',
    'port' => '1883',
    'keep-alive' => 30,
);
$clientSetting = array(
    'id' => 'id',
    'topic' => '/haco/traffic',
    'qos' => 1,
    'user' => '',
    'password' => '',
);
// Create MQTT Client
$client = new Mosquitto\Client($clientSetting['id']);

// Set handler
$client->onConnect('connect');
$client->onDisconnect('disconnect');
$client->onSubscribe('subscribe');
$client->onMessage('message');
//$client->onLog('logger');

//Connect
$client->setCredentials($clientSetting["user"], $clientSetting["password"]);
$client->connect($brokerInfo["ip"], $brokerInfo["port"], $brokerInfo["keep-alive"]);

// Subscribe a topic
$client->subscribe($clientSetting["topic"], $clientSetting['qos']);

// Run
$client->loopForever();

function connect($rc, $message)
{
    // Success
    if ($rc == 0) {
        echo "Connected \n";
    } else {
        echo "Connection Error[code = {$rc}]: {$message} ";
    }
}
function subscribe(){
    echo "Subscribed to a topic\n";
}
function unsubscribe(){
    echo "Unsubscribed from a topic\n";
}
function message(){
    //usleep(1000);
}
function disconnect(){
    echo "Disconnected cleanly\n";
}
?>