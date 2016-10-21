//
//  ORBMqttClient.m
//  
//
//  Created by max on 2016/6/23.
//
//

#import "ORBMqttClient.h"
#import "MQTTSession.h"
#import "MQTTDecoder.h"
#import "MQTTSessionLegacy.h"
#import "MQTTSessionSynchron.h"
#import "MQTTMessage.h"
#import "MQTTTransport.h"
#import "MQTTCFSocketTransport.h"
#import "MQTTCoreDataPersistence.h"
#import "MQTTSSLSecurityPolicyTransport.h"

//#import "ORBApp.h"
//#import "ORBCached.h"
//
//#import "kitConfig.h"

@interface ORBMqttClient(){
    MQTTSession *sessionClient;
    NSString *prefix_topic;
    
    bool dup_status;
}

@end

@implementation ORBMqttClient
@synthesize _delegate;
BOOL isLog = NO;

ORBMqttClient *mqtt;
+ (ORBMqttClient *)configure{
    
    if (mqtt==nil) {
        NSLog(@"====================");
        NSLog(@"Welcome to [%@]",mqttPrefix);
        NSLog(@"====================");
        mqtt = [[self alloc] init];
    }
    return mqtt;
}

- (BOOL)is_login{
    if ([[[ORBApp configure] get_orbwebUTK] isEqualToString:@""]) {
        return NO;
    }
    return YES;
}

#pragma mark - connect
- (void)connectMqttWithEmail:(NSString *)email password:(NSString *)pw company_name:(NSString *)companyN uuid:(NSString *)uuid
{
    if (![self is_login]) {
        return;
    }
    
    if (sessionClient.status == MQTTSessionStatusConnected) {
        return;
    }
    
//    NSString *clientId = [NSString stringWithFormat:@"%@-%@",companyN,uuid];
    NSString *clientId = [NSString stringWithFormat:@"%@",uuid];
    [self set_topic_prefix:companyN];
    
    [self disconnectMqtt];
    sessionClient = [[MQTTSession alloc] init];
    sessionClient.userName = email;
    sessionClient.password = pw;
    sessionClient.clientId = clientId;
    sessionClient.keepAliveInterval = 60;
    sessionClient.cleanSessionFlag = NO;
    sessionClient.willFlag = false;
    sessionClient.willTopic = nil;
    sessionClient.willMsg = nil;
    sessionClient.willQoS = 0;
    sessionClient.willRetainFlag = NO;
    sessionClient.protocolLevel = 4;
    sessionClient.runLoop = [NSRunLoop currentRunLoop];
    sessionClient.runLoopMode = NSDefaultRunLoopMode;
    sessionClient.securityPolicy = nil;
    sessionClient.certificates = nil;

    //
    MQTTCoreDataPersistence *persistence = [[MQTTCoreDataPersistence alloc] init];
    
    persistence.persistent = NO;
    persistence.maxWindowSize = 16;
    persistence.maxSize = 67108864;
    persistence.maxMessages = 1024;
    
    sessionClient.persistence = persistence;
    
    //
    NSString *urlHost = [mqttPrefix stringByAppendingString:mqttPostfix];
    MQTTCFSocketTransport *transport = [[MQTTCFSocketTransport alloc] init];
    transport.host = urlHost;
    transport.port = 1883;

    sessionClient.transport = transport;
    
    [sessionClient connect];
    
#pragma mark - 回調處理
    __block ORBMqttClient *block_self = self;
//    __block NSObject<mqttDelegate> *block_delegate = _delegate;
    
#pragma mark 處理連線狀態
    sessionClient.connectHandler = ^(NSError *error){
        if (error) {
            NSLog(@"====== connectHandler error : %@ ======",error);
            [block_self responseConnectError:error];
        }
    };
    
#pragma mark 處理所有事件
    
    sessionClient.connectionHandler = ^(MQTTSessionEvent event){
        NSLog(@"====== MQTTSessionEvent : %ld ======", (long)event);
        [block_self responseSessionEvent:event];
    };
    
#pragma mark 處理訊息
    sessionClient.messageHandler = ^(NSData* message, NSString* topic){
        NSString* newStr = [[NSString alloc] initWithData:message encoding:NSUTF8StringEncoding];
        NSLog(@"====== Topic : %@ ; Msg : %@ ======",topic,newStr);
        [block_self responseMsg:message topic:topic];
    };
}

#pragma mark - disconnect
- (void)disconnectMqtt{
    [sessionClient disconnect];
}

#pragma mark - 訂閱
#pragma mark 設定 topic

- (void)set_topic_prefix:(NSString *)companyN{
    prefix_topic = [NSString stringWithFormat:@"/%@/ipcam/motiondetector",companyN];
}

- (NSString *)set_topic_suffix_with_sid:(NSString *)sid{
    return [NSString stringWithFormat:@"%@/%@",prefix_topic,sid];
}

#pragma mark subscribe
- (void)subscribeTopicToMQTT:(NSString *)sid
                     complete:(void (^)(bool status,NSString *response))complete{
    
    if (![self is_login]) {
        return;
    }
    
    NSString *topic = [self set_topic_suffix_with_sid:sid];
    
    /*
     判斷 topic 是否已經被訂閱過
     */
    
    if ([ORBCached isCached_topic_of_sid:sid]) {
        // 已訂閱
        NSLog(@"====Topic is subscribed====");
    }
    else{
        [sessionClient subscribeToTopic:topic
                                atLevel:0
                       subscribeHandler:^(NSError *error, NSArray<NSNumber *> *gQoss){
                           if (error) {
                               complete(NO,error.localizedDescription);
                           } else {
                               
                               NSLog(@"====Subscription sucessfull====");
                               NSString *s = [NSString stringWithFormat:@"Subscription sucessfull, Qos : %@", gQoss];
                               complete(YES,s);
                               
                               [ORBCached cached_topic:topic andSid:sid];
                           }
                       }];
    }
    
}

#pragma mark unsubscribe
- (void)unsubscribeTopicToMQTT:(NSString *)sid
                      complete:(void (^)(bool status,NSString *response))complete{
    
    if (![self is_login]) {
        return;
    }
    
    NSString *topic = [self set_topic_suffix_with_sid:sid];
    
    [ORBCached delete_topic_of_sid:sid];
    
    [sessionClient unsubscribeTopic:topic
                 unsubscribeHandler:^(NSError *error){
                     if (error) {
                         NSLog(@"====UnSubscription failure====");
                         complete(NO,error.localizedDescription);
                     }
                     else{
                         NSLog(@"====UnSubscription sucessfull====");
                         complete(YES,@"Unsubscribe successful");
                     }
    }];
}


#pragma mark - SessionEvent 處理
- (void)responseSessionEvent:(MQTTSessionEvent) event{
    
    [_delegate passSessionEvent:(long)event];
    
    if (event == MQTTSessionEventConnected) {
    }
    else if (event == MQTTSessionEventConnectionRefused){
    }
    else if (event == MQTTSessionEventConnectionClosed){
    }
    else if (event == MQTTSessionEventConnectionError){
    }
    else if (event == MQTTSessionEventProtocolError){
    }
    else if (event == MQTTSessionEventConnectionClosedByBroker){
    }
}

#pragma mark - 處理所有連線錯誤事件
- (void)responseConnectError:(NSError *)error{
    [_delegate passError:error];
}

#pragma mark - 處理訊息接收事件
- (void)responseMsg:(NSData *)message topic:(NSString*)topic{
    [_delegate passReceiveNewMsg:message topic:topic];
}

#pragma mark - mqtt session status
- (CMQTTSessionStatus)mqttSessionStatus{
    
    CMQTTSessionStatus temp;
    if (sessionClient.status == MQTTSessionStatusCreated) {
        temp = CMQTTSessionStatusCreated;
    }
    else if (sessionClient.status == MQTTSessionStatusConnecting){
        temp = CMQTTSessionStatusConnecting;
    }
    else if (sessionClient.status == MQTTSessionStatusConnected){
        temp = CMQTTSessionStatusConnected;
    }
    else if (sessionClient.status == MQTTSessionStatusDisconnecting){
        temp = CMQTTSessionStatusDisconnecting;
    }
    else if (sessionClient.status == MQTTSessionStatusClosed){
        temp = CMQTTSessionStatusClosed;
    }
    else if (sessionClient.status == MQTTSessionStatusError){
        temp = CMQTTSessionStatusError;
    }
    
    return temp;
}

#pragma mark -
- (void)showAllSubscribeTopics{
    NSLog(@"%@",[ORBCached load_topics]);
}

#pragma mark - 測試 mqtt
- (void)testMqttWithSid:(NSString *)sid{
    
    NSString* str = @"Hello World!";
    NSData* data = [str dataUsingEncoding:NSUTF8StringEncoding];
    [sessionClient publishData:data
                       onTopic:[self set_topic_suffix_with_sid:sid]
                        retain:NO
                           qos:0];
}

@end
