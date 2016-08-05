# install
- [github](https://github.com/emqtt/emqttd)
	- 此為源碼，必須經過 compile，會在 rel 中產生 emqttd 的檔案
	- emqttd(1.1.2)
- 文件 : http://docs.emqtt.com/en/latest/getstarted.html

## ubuntu

- aws ubuntu
- must
	- git
	- make
	- `sudo apt-get install erlang`
	- erlang 17+
		- 檢查版本指令 `$erl`
- make error : `ERROR: OTP release R17 or later is required, you have: R16B03`
	- https://gist.github.com/bryanhunter/10380945
		- `http://erlang.org/download/` 可去此地方下載最新版本
	- 若為 mac，則直接 `brew install --devel erlang` 安裝最新版本的 erlang 即可
	- 安裝後，須將預設的 `/usr/bin/erl` 變更連結位置

## centOs
- env 
	- CentOS Linux release 7.2.1511 (Core) 
- [install erlang]
	- 套件安裝
		1. `wget https://packages.erlang-solutions.com/erlang-solutions-1.0-1.noarch.rpm`
		2. `rpm -Uvh erlang-solutions-1.0-1.noarch.rpm`
		3. `sudo yum install esl-erlang`
	- 使用 `sudo yum install esl-erlang` : 可拿到 R19 版本
	- 其他參考 : 
		- 下載開發版本 : [erlang.org](http://www.erlang.org/downloads)
		- 下載各種載具穩定版本 [erlang-solutions](https://www.erlang-solutions.com/resources/download.html)
		- 若用 `sudo yum install erlang` : 版本為 R16B-03.16.el7  
		- 若要移除錯誤版本 `sudo yum remove erlang*`

## mac osx 

- env : 
	- mac osx 10.11.5
	- homebrew 0.9.9
- cmd

	```
	brew install erlang-r19 
	git clone https://github.com/emqtt/emqttd.git
	cd emqttd && make && make dist
	cd rel/emqttd && ./bin/emqttd start
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
	- mosquitto_sub -h dev-mqtt.orbwebsys.com -t /hello/# -i sub -u jhaoheng@gmail.com -P a3456
	- mosquitto_sub -h dev-mqtt.orbwebsys.com -t /hello/# -i sub -u orbweb@orbweb.com -P orbweb@orbweb.com
- `mosquitto_pub -h [host] -t [topic] -m "[message]" -u [user] -P [password] -i [clientId]`
	- mosquitto_pub -h dev-mqtt.orbwebsys.com -t /hello/123 -m "world" -u orbweb@orbweb.com -P orbweb@orbweb.com -i pub

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