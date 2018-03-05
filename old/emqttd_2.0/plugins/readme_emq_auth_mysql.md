# emqttd_2.0 install

```
git clone https://github.com/emqtt/emqttd-relx.git
cd emqttd-relx && make
cd _rel/emqttd && ./bin/emqttd console
```

# emqttd_2.0 plugins

在 2.0 版本，plugins 放在 source/deps/ 中，一次編譯 make，就編譯 本體＋plugins
單一 plugin 可透過 make & make test 進行測試

# emq_auth_mysql

- 結構
	- etc		: 設定檔
	- include	: 此 plugins 掛載 emqttd 後的定義名稱
	- priv		: 設定檔(etc/.conf) 配對給 emqttd 的參數設定值
	- src		: plugins 本體
		- emq_acl_mysql.erl
		- emq_auth_mysql.erl     : 處理 auth 本身
		- emq_auth_mysql_app.erl : 負責處理 plugins 掛載/停止，並將 etc 設定檔放入位置
		- emq_auth_mysql_cli.erl : sql 解析
		- emq_auth_mysql_sup.erl 
- emqttd_auth_mod.erl : 說明 auth 在處理過程中的順序，包含 `[init/1, check/3, description/0]`
- 測試
	1. 在 plugins 執行 `make & make tests`，會產生
		- `.erlang.mk/`
		- `deps`
		- `ebin`
	2. 安裝 emqttd 
		1. `git clone https://github.com/emqtt/emqttd-relx.git`
		2. `cd emqttd-relx && make`
		3. `cd _rel/emqttd && ./bin/emqttd console`
	3. 安裝 plugins 
		- 方法一 : 在 `deps` 刪除此 plugin ，並複製到該位置，重新執行 make
		- 方法二 : 在 emqttd-relx 中，可找到 Makefile 中 dep_emq_auth_mysql 的位置，替換掉，重新執行 make
	4. 實機測試
		1. emqttd plugin 安裝完畢後，執行 make 編譯 emqttd。
		2. `cd _rel/emqttd/bin`
		3. `./emqttd stop;./emqttd start`
		4. `./emqttd_ctl plugins load emq_auth_mysql` : 注意資料庫要正確，不然會出現錯誤
		5. `./emqttd stop`
		6. `./emqttd console`
		7. 測試指令 : 
			- user : `mosquitto_sub -h localhost -t /orbweb/# -i client_id_2 -u max.hu@orbweb.com -P a12345`
			- device : `mosquitto_sub -h localhost -t /orbweb/# -i ccfc821be2c7b2a39133f7f7cbfa8106fcd42def48faf1e3c7 -u 00:1B:FE:0D:44:21 -P hcpwdtst`

			
## ACL 

- 結構順序
	- 所有的進程順序 `deps/emqttd/src/emqttd_acl_mod.erl`
		- 指令(mosquitto)參數取得的方式，透過此地方配置
	- {plugin}/etc/emq_auth_mysql_cli : 處理 sql，並將 變數 與 query 結合的地方，手動設置 %u....
	- {plugin}/etc/emq_auth_mysql.conf : 設定 acl_query 與 acl_nomatch
	- {plugin}/etc/emq_auth_mysql_app.erl : 負責取得從 conf 的 query 參數，將之放入 mod 中，進行管控
		- 裡面有 AclEnv : 可將任意參數放置其中
	- {plugin}/etc/emq_acl_mysql.erl : 

