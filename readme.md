# readme

- 使用 emqttd(1.1.1) : http://docs.emqtt.com/en/latest/getstarted.html
- 0.12 : 常用指令 & 設定
- centOs 7.x

## emqttd broker 控制

```
./bin/emqttd reboot
./bin/emqttd stop
./bin/emqttd start
```

## plugin

```
|-/plugins/emqttd_auth_http
	|- ebin/
	|- etc/
	|- include/
	|- src/
```
### compile with [rebar]

- 使用 `./rebar compile` compile `include` 與 `src` 檔案，產生出 `ebin/`
- 主要的 .erl file 放於 src 中
- `etc/` 負責處理 auth 設定傳送的出去的格式

### 編寫注意事項

- 每次編譯前，請先刪除舊有 `ebin/`
- 編譯後，安全起見，進行 `./bin/emqttd reboot` & `./bin/emqttd start`
- 其中 `emqttd_acl_http.erl` 若不想執行，請勿 compile or 關掉設定

**Notice : 當每次變更 plugin 的任何設定，都要重新啟動 MQTT broker**

### cmd

- list all plugins status : `./bin/emqttd_ctl plugins list`
- 啟動 plugin : `./bin/emqttd_ctl plugins load [plugin name]`
- 停止 plugin : `./bin/emqttd_ctl plugins unload [plugin name]`

### auth 設定 http api

- auth : 只有在連線時，才會被觸發
	- 若不想重複認證，請在 `etc/` 中選擇一種連線方式

[官方 emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)

1. `vi emqttd/plugins/emqttd_auth_http/etc`
2. find `url` and fill it.
3. 重新啟動 MQTT broker

#### http post 可取得的參數，可參考系統設定

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

### 關於 auth plugin response

根據官方文件 [emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)
必須要用 http status code 回傳給 emqttd auth 

- php 用法 

```
http_response_code(200);//200:success ; 401:failure
echo http_response_code();
```

- 一旦 failure 則 device 斷線，無法連接

