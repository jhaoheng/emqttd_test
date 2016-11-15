# install
- [github](https://github.com/emqtt/emqttd)
- install : <http://emqtt.io/docs/v2/install.html>

## erlang
### ubuntu

- aws ubuntu
- must
	- git
	- make
	- `sudo apt-get install erlang`
	- erlang 17+
		- 檢查版本指令 `$erl`

### centOs
- env 
	- CentOS Linux release 7.2.1511 (Core) 
- [install erlang]
	- 使用 `sudo yum install esl-erlang` : 可拿到 R19 版本
	- 其他參考 : 
		- 下載開發版本 : [erlang.org](http://www.erlang.org/downloads)
		- 下載各種載具穩定版本 [erlang-solutions](https://www.erlang-solutions.com/resources/download.html)
		- 若用 `sudo yum install erlang` : 版本為 R16B-03.16.el7  
		- 若要移除錯誤版本 `sudo yum remove erlang*`

### mac osx 

- env : 
	- mac osx 10.11.5
	- homebrew 0.9.9
- cmd

	```
	brew install erlang-r19 
	```

# 查看版本 & Troubleshoot

- 查看版本 `erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell`
- Troubleshoot
	- 若無先安裝 erlang，在 compile 時會產生 escript 找不到的問題
	- 需要 `release R17` 以上的版本 

# 開啟 browser dashboard

http://localhost:18083

# 測試方法 : mosquitto
- 安裝 mosquitto 
	- mac : `brew install mosquitto`
- `mosquitto_sub -h [host] -t [topic] -i [clientId] -u [user] -P [password]`
- `mosquitto_pub -h [host] -t [topic] -m "[message]" -u [user] -P [password] -i [clientId]`

# compile plugin
## 安裝 rebar

- 一樣要先安裝 erlang
- 目的 : compile plugin file
- 需要 source
	- emqttd/include
	- plugins/src
- 建議直接 compile source，再將 變更的 plugins 移到正式環境中


### 編寫注意事項

- 每次編譯前，請先刪除舊有 `ebin/`
- 編譯後，安全起見，進行 `./bin/emqttd reboot` & `./bin/emqttd start`
- 其中 `emqttd_acl_http.erl` 若不想執行，請勿 compile or 關掉設定

**Notice : 當每次變更 plugin 的任何設定，都要重新啟動 MQTT broker**

## 設定 plugin

1. 要確定 reload plugin 啟動
2. 確定是否已經啟動
3. 要使用 `rebar` 指令建立 plugin 初始化

### 透過 plugin auth 進行驗證管控方法

- 根據官方文件 [emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)，必須要用 http status code 回傳給 emqttd auth
- php 用法 : http://php.net/manual/en/function.http-response-code.php
	- 401 : `http_response_code (401)` => Auth failure
	- 200 : `http_response_code (200)` => Auth success

### http post 可取得的參數，可參考系統設定

- 'username'
- 'password'
- 'clientid'
- 'access' = Qos
- 'ipaddr'
- 'topic'
- ....其他

ex: 

- 第一次 [connect]

```
  'username' => 'admin'
  'password' => 'public'
  'clientid' => 'bib'
  'access' => NULL
  'ipaddr' => NULL
  'topic' => NULL
```

# 使用 mqtts:8883 來登入 emqttd

## 使用 repo 中的 generate-CA.sh 產生憑證

1. 使用 generate-CA.sh
2. sh generate-CA.sh
3. ca.xxx 自己的證書
4. xxxx.crt, xxxx.csr, xxxx.key 透過 ca 證書簽發出的憑證，xxxx 為產生憑證的電腦名稱
5. 透過 xxxx.csr / xxxx.key 改為 ssl.csr / ssl.key 放在 emqttd/etc/ssl 中

## 使用 cmd

1. 產生自簽憑證
	- `openssl req -newkey rsa:2048 -x509 -nodes -sha512 -days 365 -extensions v3_ca -keyout ca.key -out ca.crt`
		- check : `openssl x509 -in ca.crt -nameopt multiline -subject -noout`
		- 此時輸入的 commonName 可為任意參數
2. 產生 server 資訊
	1. 產生 server key
		- `openssl genrsa -out server.key 2048`
	2. 由 server key 產生出 csr ，含 基本資訊與 public key
		- commonName 在 localhost 時請填寫 localhost，若非 localhost 請填寫該 dns / ip
		- 注意: ca.crt 與 server.crt 的 commonName 不可相同
		- `openssl req -new -sha512 -out server.csr -key server.key`
3. 由 自簽憑證(ca.crt & ca.key) 去簽 server.csr，產生出 server.crt
	- `openssl x509 -req -sha512 -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -CAserial ca.srl -out server.crt -days 365 -extensions JPMextensions`
	- check : `openssl x509 -in server.crt -nameopt multiline -subject -noout`

		```
		subject= 
		    countryName               = TW
		    stateOrProvinceName       = TAIWAN
		    localityName              = TAIPEI
		    organizationName          = COMPANY
		    organizationalUnitName    = DEV
		    commonName                = dev-mqtt.com
		    emailAddress              = max.hu@mail
		```
4. 重複 (2),(3) 產生 client.crt , client.key，此 client 表示另一台 連接的機器


## 驗證: check server.crt is generated by ca.crt 

- `openssl verify -CAfile ca.crt server.crt`
	- output ssl.crt: OK
- `openssl verify -CAfile ca.crt client.crt`
	- output ssl.crt: OK

## 測試

1. 將 server.crt -> ssl.crt 與 server.key -> ssl.key，放到 emqttd/etc/ssl
2. 測試
	1. 測試 自簽憑證(CA)
		- `mosquitto_sub -h [server_ip] -p 8883 -t [topic] -i [clientId] -u [username] -P [password] --cafile ca.crt`
	2. 測試 server.crt
		- `mosquitto_sub -h [server_ip] -p 8883 -t [topic] -i [clientId] -u [username] -P [password] --cert server.crt --key server.key`
	3. 測試 client.crt
		- `mosquitto_sub -h [server_ip] -p 8883 -t [topic] -i [clientId] -u [username] -P [password] --cert client.crt --key client.key`
3. 進入 emqttd:18083 查看是否登入成功

## ios framewrok

- [ckrey MQTT-Client-Framework](https://github.com/ckrey/MQTT-Client-Framework)
- 使用 .der
	- `mosquitto_sub -h 127.0.0.1 -p 8883 -t {topic} -i {client} -u {user} -P {pw} --cert server.crt --key server.key` : 先檢查 server.crt 與 key 是否正確
	- `openssl x509 -in server.crt -out server.pem -outform PEM` : 從 server.crt 轉 pem
	- `openssl x509 -outform der -in server.pem -out server.der` : 從 pem 轉 der
- set server.der to `MQTTSSLSecurityPolicy` : 參考 ios app

# emqttd 使用心得
## EMQTTD (MQTT broker) Feature

- 完整的MQTT V3.1/V3.1.1協議規範支持
- QoS0, QoS1, QoS2消息支持
- 持久會話與離線消息支持
- Retained消息支持
- Last Will消息支持
- TCP/SSL連接支持
- MQTT/WebSocket(SSL)支持
- HTTP消息發布接口支持
- $SYS/#系統主題支持
- 客戶端在線狀態查詢與訂閱支持
- 客戶端ID或IP地址認證支持
- 用戶名密碼認證支持
- LDAP認證
- Redis、MySQL、PostgreSQL認證集成
- 瀏覽器Cookie認證
- 基於客戶端ID、IP地址、用戶名的訪問控制(ACL)
- 多服務器節點集群(Cluster)
- 多服務器節點橋接(Bridge)
- mosquitto橋接支持
- Stomp協議支持
- Stomp/SockJS支持
- 通過Paho兼容性測試

- 說明 : http://emqttd-docs.readthedocs.io/en/latest/getstarted.html

## 其他尚未整理

### hook
http://emqtt.com/docs/design.html#hook

```
1. 必須先讀取 module(emqttd_plugin_template)
2. export 為輸出的順序列
3. 使用 hook 前，需要先設定啟動 plugin
```
- 若要檢查 hook 的話，試試 https://github.com/emqtt/emqttd/blob/master/src/emqttd_hook.erl#L83
