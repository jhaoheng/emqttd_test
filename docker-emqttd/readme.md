# 分為兩種方法

1. buildFromMax
2. buildFromOffice

# 透過 docker 建立，會產生的問題
- 在 low-memory 下，啟動速度過慢，或者容易造成 crash，使得 health-check 一直 ping 不到
- 此問題不是 image base 的問題，因為 alpine 與 ubuntu:16.04 都會有一樣的問題在 aws 上
- aws 規格 : 
  - t2.small : 建立失敗. 1 vcpu, 2 GiB memory
  - t2.medium : 建立成功. 2 vcpu, 4 GiB memory
- 所以建議還是直接透過安裝包，直接安裝會比較好
- 但如此一來，部署就比較不方便

# buildFromMax

- 用 ubuntu:16.04 當 base image
- db 使用 mysql

# buildFromOffice

- 透過官方的 github 建立的版本, alpine:v3.7
- db 使用 mysql