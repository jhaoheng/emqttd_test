# readme

- 使用 emqttd(1.1.1) : http://docs.emqtt.com/en/latest/getstarted.html
- 0.1 : 常用指令 & 設定
- centOs 7.x

## emqttd broker 控制

```
./bin/emqttd reboot
./bin/emqttd stop
./bin/emqttd start
```

## plugin

### cmd

- list all plugins status : `./bin/emqttd_ctl plugins list`
- 啟動 plugin : `./bin/emqttd_ctl plugins load [plugin name]`
- 停止 plugin : `./bin/emqttd_ctl plugins unload [plugin name]`

**Notice : 當每次變更 plugin 的任何設定，都要重新啟動 MQTT broker**

### auth 設定 http api

- auth : 只有在連線時，才會被觸發
	- 第一次發送時，會有兩個動作
		1. connect
		2. publish

[官方 emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)

1. `vi emqttd/plugins/emqttd_auth_http/etc`
2. find `url` and fill it.
3. 重新啟動 MQTT broker

#### http post 可取得的參數
- 'username'
- 'password'
- 'clientid'
- 'access' = Qos
- 'ipaddr'
- 'topic'

ex: 

- 第一次 [connect]
```
array (
  'username' => 'admin',
  'password' => 'public',
  'clientid' => 'bib',
  'access' => NULL,
  'ipaddr' => NULL,
  'topic' => NULL,
)
```
- 第一次 [publish]
```
array (
  'username' => 'admin',
  'password' => NULL,
  'clientid' => 'bib',
  'access' => '2',
  'ipaddr' => '59.124.2.170',
  'topic' => 'test',
)
```