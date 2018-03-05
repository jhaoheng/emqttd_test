<?php  

use Phalcon\Queue\Beanstalk;

$tube = "testTube_1";
beanstalk_put($tube);
$tube = "testTube_2";
beanstalk_put($tube);
$tube = "testTube_3";
beanstalk_put($tube);
$tube = "testTube_4";
beanstalk_put($tube);

function beanstalk_put($tube){
    $queue = new Beanstalk(
        [
            "host" => "localhost",
            "port" => "11300",
            'persistent' => true,
        ]
    );

    
    $queue->choose($tube);
    $queue->watch($tube);

    $attributes = array('priority'  => 0,
                        'delay'     => 0, 
                        // "ttr"       => 3600
                        );

    $jobContent = "hello : ".$tube;
    $jobId =$queue->put($jobContent, $attributes);
    echo $jobId.PHP_EOL;
}
?>