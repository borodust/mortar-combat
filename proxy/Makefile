WORK_DIR="$(dir $(abspath $(lastword $(MAKEFILE_LIST))))"
BUILD_DIR=$(WORK_DIR)/build


MANIFEST_FILE=$(BUILD_DIR)/manifest



build: image_archive


prepare:
	mkdir -p $(BUILD_DIR)/{server,}
	cp $(WORK_DIR)/Dockerfile $(BUILD_DIR)/server/Dockerfile

manifest: prepare
	sbcl --non-interactive									\
	     --eval "(ql:write-asdf-manifest-file \"$(MANIFEST_FILE)\" :if-exists :supersede)"

executable: prepare manifest
	buildapp --output $(BUILD_DIR)/server/server.bin	\
		 --entry "mortar-combat.proxy::main"		\
		 --manifest-file $(MANIFEST_FILE)		\
		 --load-system "mortar-combat/proxy"		\
		 --compress-core

image: executable
	cd $(BUILD_DIR)/server/ && docker build -t mortar-combat/mortar-combat-proxy:latest .

image_archive: image
	docker save mortar-combat/mortar-combat-proxy > $(BUILD_DIR)/mortar-combat-proxy.img
	cd $(BUILD_DIR)/ && zip -r server.zip mortar-combat-proxy.img
	rm -f $(BUILD_DIR)/mortar-combat-proxy.img


logs:
	docker logs mortar-combat-proxy


container:
	docker create --name mortar-combat-proxy -p 8778:8778 -p 8222:8222	\
		   mortar-combat/mortar-combat-proxy

serve:
	docker start mortar-combat-proxy

stop:
	docker stop mortar-combat-proxy


clean:
	rm -rf $(BUILD_DIR)
