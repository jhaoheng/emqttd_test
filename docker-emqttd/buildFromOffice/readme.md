# 目的

- 透過 docker，直接建立一個依賴 mysql 的 emq-container

# 建立&使用步驟
1. 從 https://github.com/emqtt/emq-docker ，建立 emq-docker image
  1. `git clone -b master https://github.com/emqtt/emq_docker.git`
    - 注意這邊在 2.3.5 的版本中，start.sh 在 141 行後，需增加一個 sleep 5 秒的動作，不然機器起不來，會錯誤
    - 有回報給作者，下一個版本如果沒修正，記得要修訂 start.sh，才會比較正常運作
  2. `docker build -t emq:vx.x.x .`
  3. 記得將測試好的版本放到 docker registry(hub) 中
2. 調整 docker-compose 中，emq 使用的 image name & version。確認環境變數有對應到目前的版本。
3. 執行
  1. 測試 `docker-compose up`，正常後
  2. 執行 `docker-compose up -d`

# 關於 database

- 若 database 使用 docker 建置，必須使用 mysql:5.6 
- mysql:5.7 與 mariadb 版本在 docker 中使用，會產生 `Got an error reading communication packets`

# 關於測試方法

## mosquitto

- 安裝 mosquitto
  - mqtt 所有的建議 client/server 工具 : https://github.com/mqtt/mqtt.github.io/wiki/tools
  - 下載 mosquitto : http://mosquitto.org/download/
    - osx 安裝 mosquitto : `brew install mosquitto`,  測試安裝版本 1.4.14
- 測試
  - mosquitto_pub
    - basic : `mosquitto_pub -t {} -m {} -h {} -p {}`
    - detail : `mosquitt_pub --help`
  - mosquitto_sub
    - basic : `mosquitto_sub -t {topic} -h {host} -p {port}`
    - detail : `mosquitto_sub --help`