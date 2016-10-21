//
//  ORBMqttClient.h
//  
//
//  Created by max on 2016/6/23.
//
//

#import <Foundation/Foundation.h>

/*
 狀態
 */
typedef NS_ENUM(NSInteger, CMQTTSessionStatus) {
    CMQTTSessionStatusCreated,
    CMQTTSessionStatusConnecting,
    CMQTTSessionStatusConnected,
    CMQTTSessionStatusDisconnecting,
    CMQTTSessionStatusClosed,
    CMQTTSessionStatusError
};

/*
 事件
 */
typedef NS_ENUM(NSInteger, ClientMQTTSessionEvent) {
    CMQTTSessionEventConnected,
    CMQTTSessionEventConnectionRefused,
    CMQTTSessionEventConnectionClosed,
    CMQTTSessionEventConnectionError,
    CMQTTSessionEventProtocolError,
    CMQTTSessionEventConnectionClosedByBroker
};

@protocol mqttDelegate

- (void)passSessionEvent:(ClientMQTTSessionEvent)event;
- (void)passError:(NSError *)error;
- (void)passReceiveNewMsg:(NSData *)message topic:(NSString*)topic;

@end

@interface ORBMqttClient : NSObject{
    NSObject<mqttDelegate> *_delegate;
}
@property (nonatomic, retain) NSObject<mqttDelegate> *_delegate;

/*
 init
 */
+ (ORBMqttClient *)configure;

/*
 - email
 - password
 - company
 - uuid
 if leave mqtt, must disconnect Mqtt first.
 */

//"- (void)connectMqttWithEmail: password: clientId:" -- DEPRECATED;
- (void)connectMqttWithEmail:(NSString *)email password:(NSString *)pw company_name:(NSString *)companyN uuid:(NSString *)uuid;

- (void)disconnectMqtt;

/*
 subscribe : 連線成功才能訂閱，請用 delegate : passSessionEvent 獲取事件來源
 */
- (void)subscribeTopicToMQTT:(NSString *)sid
                    complete:(void (^)(bool status,NSString *response))complete;

- (void)unsubscribeTopicToMQTT:(NSString *)sid
                      complete:(void (^)(bool status,NSString *response))complete;

- (void)showAllSubscribeTopics;
/*
 status
 */
- (CMQTTSessionStatus)mqttSessionStatus;

/*
 Test MQTT : will send "Hello World!" to Topic
 */
- (void)testMqttWithSid:(NSString *)sid;

extern BOOL isLog;

@end
