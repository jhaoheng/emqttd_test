//
//  ViewController.m
//  mqtt
//
//  Created by max on 2016/10/20.
//  Copyright © 2016年 max hu. All rights reserved.
//

#import "ViewController.h"

#import "MQTTSession.h"
#import "MQTTDecoder.h"
#import "MQTTSessionLegacy.h"
#import "MQTTSessionSynchron.h"
#import "MQTTMessage.h"
#import "MQTTTransport.h"
#import "MQTTCFSocketTransport.h"
#import "MQTTCoreDataPersistence.h"
#import "MQTTSSLSecurityPolicyTransport.h"

@interface ViewController (){
    
    MQTTSession *session;
}

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.
    
    NSString *mail = @"max.hu@orbweb.com";
    NSString *pw = @"a12345";
    NSString *uuid = @"test";
    BOOL isSSL = NO;
    [self connectMqttWithEmail:mail password:pw uuid:uuid ssl:isSSL];
    
    
    UIButton *btn = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    btn.frame = CGRectMake(0, 0, 100, 100);
    [btn setTitle:@"test" forState:UIControlStateNormal];
    [btn addTarget:self action:@selector(do:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:btn];
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - 
#pragma mark - connect
- (void)connectMqttWithEmail:(NSString *)email
                    password:(NSString *)pw
                        uuid:(NSString *)uuid ssl:(BOOL)isSSL
{
    MQTTSSLSecurityPolicy* securityPolicy = [MQTTSSLSecurityPolicy policyWithPinningMode:MQTTSSLPinningModePublicKey];
    NSString* certificate = [[NSBundle bundleForClass:[self class]] pathForResource:@"server" ofType:@"der"];
    securityPolicy.pinnedCertificates = @[ [NSData dataWithContentsOfFile:certificate] ];
    securityPolicy.allowInvalidCertificates = YES; // we using self-signed certificate and didn't coupled with CA infrastructure
    
    int port = 8883;
    if (!isSSL) {
        securityPolicy = nil;
        port = 1883;
    }
    
    session = [[MQTTSession alloc]
                            initWithClientId:@"MQTTOverTLS"
                            userName:email
                            password:pw
                            keepAlive:60
                            cleanSession:YES
                            will:YES
                            willTopic:@"example/status"
                            willMsg:[@"Client off-line" dataUsingEncoding:NSUTF8StringEncoding]
                            willQoS:2
                            willRetainFlag:YES
                            protocolLevel:4
                            runLoop:[NSRunLoop currentRunLoop]
                            forMode:NSRunLoopCommonModes
                            securityPolicy:securityPolicy];
    
    [session connectAndWaitToHost:@"localhost" port:port usingSSL:isSSL];
    // do business...
//    [session closeAndWait];
    
    
    [session connect];
    
}

- (void)do:(id)sender
{
    
    NSData* data = [@"hello" dataUsingEncoding:NSUTF8StringEncoding];
    [session publishAndWaitData:data
                        onTopic:@"/topic"
                         retain:NO
                            qos:MQTTQosLevelAtLeastOnce];
}


@end
