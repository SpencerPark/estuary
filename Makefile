# If 'rsync' installed, use it to perform copies which only update if newer
# otherwise falling back to a plain 'cp'.
RSYNC_EXISTS := $(shell rsync --version 2>/dev/null)
#ifdef RSYNC_EXISTS
#CP=rsync  --perms --executability
#CP_RECURSIVE=rsync --recursive --perms --executability
#else
CP=cp
CP_RECURSIVE=cp -Rf
#endif
STACK_SERVER=cd server/ && stack

assertInNixShell:
ifndef IN_NIX_SHELL
	$(error Must be run in a nix shell)
endif

assertInNixGhcjsShell:
ifndef NIX_GHCJS
	$(error Must be run in the nix ghcjs shell. "nix-shell -A shells.ghcjs")
endif

assertInNixGhcShell:
ifndef NIX_GHC
	$(error Must be run in the nix ghc shell. "nix-shell -A shells.ghc")
endif

cabalBuildClient: assertInNixGhcjsShell
	@ echo "cabalBuildClient:"
	cd client && hpack --force
	cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all --disable-library-profiling --disable-documentation

nixShellBuildClient:
	nix-shell -A shells.ghcjs --run "make cabalBuildClient"

cabalBuildServer: assertInNixGhcShell
	@ echo "cabalBuildServer:"
	cd common && hpack --force
	cd server && hpack --force
	cabal new-build all --disable-library-profiling --disable-documentation

nixShellBuildServer:
	nix-shell -A shells.ghc --run "make cabalBuildServer"

nixBuild:
	@ echo "nixBuild:"
	nix-build

PROD_STAGING_ROOT=staging/
DEV_STAGING_ROOT=dev-staging/
STAGING_ROOT=$(PROD_STAGING_ROOT)
prepStage:
	@ echo "prepStage:"
	-mkdir $(STAGING_ROOT)
	-mkdir $(STAGING_ROOT)/Estuary.jsexe/
prepDevStage: STAGING_ROOT=$(DEV_STAGING_ROOT)
prepDevStage: prepStage

cleanStage:
	@ echo "cleanStage:"
	-rm -rf $(STAGING_ROOT)
cleanDevStage: STAGING_ROOT=$(DEV_STAGING_ROOT)
cleanDevStage: cleanStage

stageStaticAssets: prepStage
	@ echo "stageStaticAssets:"
	$(CP_RECURSIVE) static/*.js $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/WebDirt $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/css-custom $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/css-source $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/fonts $(STAGING_ROOT)/Estuary.jsexe/
	$(CP_RECURSIVE) static/icons $(STAGING_ROOT)/Estuary.jsexe/

devStageStaticAssets: STAGING_ROOT=$(DEV_STAGING_ROOT)
devStageStaticAssets: stageStaticAssets

stageSamples: prepStage
	@ echo "stageSamples:"
	$(CP_RECURSIVE) static/samples/ $(STAGING_ROOT)/Estuary.jsexe/samples/
devStageSamples: STAGING_ROOT=$(DEV_STAGING_ROOT)
devStageSamples: stageSamples

GCC_PREPROCESSOR=gcc -E -x c -P -C -nostdinc
TEMPLATE_SOURCE=static/index.html.template

GET_CABAL_CLIENT_PACKAGE_NAME=python3 -c "import yaml; p = yaml.load(open('client/package.yaml', 'r')); print(p.get('name') + '-' + p.get('version', '0.0.0'), end='')"
GET_GHCJS_VERSION=ghcjs --version | sed -nre "s/.*version ([^ ]*).*/\1/p"
CABAL_CLIENT_BIN_DIR=dist-ghcjs/build/x86_64-linux/ghcjs-${GHCJS_VERSION}/${CABAL_CLIENT_PACKAGE_NAME}/x/Estuary/build/Estuary/Estuary.jsexe/
cabalStageClient: assertInNixGhcjsShell
	@ echo "cabalStageClient:"
	$(eval export CABAL_CLIENT_PACKAGE_NAME=$(shell $(GET_CABAL_CLIENT_PACKAGE_NAME)))
	$(eval export GHCJS_VERSION=$(shell $(GET_GHCJS_VERSION)))
	# compile the index.html template in development mode and stage it
	-mkdir $(DEV_STAGING_ROOT)Estuary.jsexe
	$(GCC_PREPROCESSOR) $(TEMPLATE_SOURCE) -o $(DEV_STAGING_ROOT)/Estuary.jsexe/index.html
	# stage the client js
	for part in lib out rts runmain ; do \
		$(CP) $(CABAL_CLIENT_BIN_DIR)/$$part.js $(DEV_STAGING_ROOT)/Estuary.jsexe/ ; \
		chmod a+w $(DEV_STAGING_ROOT)Estuary.jsexe/$$part.js ; \
	done

nixShellStageClient:
	nix-shell -A shells.ghcjs --run "make cabalStageClient"

GET_CABAL_SERVER_PACKAGE_NAME=python3 -c "import yaml; p = yaml.load(open('server/package.yaml', 'r')); print(p.get('name') + '-' + p.get('version', '0.0.0'), end='')"
GET_GHC_VERISON=ghc --version | sed -nre "s/.*version ([^ ]*).*/\1/p"
CABAL_SERVER_BIN=dist-newstyle/build/${system}/ghc-${GHC_VERSION}/${CABAL_SERVER_PACKAGE_NAME}/x/EstuaryServer/build/EstuaryServer/EstuaryServer
cabalStageServer: assertInNixGhcShell
	@ echo "cabalStageServer:"
	$(eval export CABAL_SERVER_PACKAGE_NAME=$(shell $(GET_CABAL_SERVER_PACKAGE_NAME)))
	$(eval export GHC_VERSION=$(shell $(GET_GHC_VERISON)))
	# stage the server binary
	$(CP) $(CABAL_SERVER_BIN) $(DEV_STAGING_ROOT)
	chmod a+w $(DEV_STAGING_ROOT)/EstuaryServer

nixShellStageServer:
	nix-shell -A shells.ghc --run "make cabalStageServer"

nixStageClient: prepStage
	@ echo "nixStageClient:"
	# compile the index.html template in production mode and stage it
	$(GCC_PREPROCESSOR) $(TEMPLATE_SOURCE) -DPRODUCTION -o $(STAGING_ROOT)/Estuary.jsexe/index.html
	# stage the minified client
	$(CP) result/ghcjs/estuary/bin/all.min.js $(STAGING_ROOT)/Estuary.jsexe/
	chmod a+w $(STAGING_ROOT)/Estuary.jsexe/all.min.js
	$(CP) result/ghcjs/estuary/bin/all.min.js.gz $(STAGING_ROOT)/Estuary.jsexe/
	chmod a+w $(STAGING_ROOT)/Estuary.jsexe/all.min.js.gz
nixDevStageClient: STAGING_ROOT=$(DEV_STAGING_ROOT)
nixDevStageClient: nixStageClient

nixStageServer: prepStage
	# stage the server binary
	@ echo "nixStageServer:"
	$(CP) result/ghc/estuary-server/bin/EstuaryServer $(STAGING_ROOT)
	chmod a+w $(STAGING_ROOT)/EstuaryServer
nixDevStageServer: STAGING_ROOT=$(DEV_STAGING_ROOT)
nixDevStageServer: nixStageServer

stackBuildServer:
	@ echo "stackBuildServer:"
	$(STACK_SERVER) setup
	$(STACK_SERVER) build

STACK_SERVER_INSTALL_DIR=$$($(STACK_SERVER) path --local-install-root)/bin/EstuaryServer

stackStageServer: stackBuildServer prepStage
	@ echo "stackStageServer:"
	$(CP) $(STACK_SERVER_INSTALL_DIR) $(STAGING_ROOT)
	chmod a+w $(STAGING_ROOT)/EstuaryServer

bundleClient: cleanStage stageStaticAssets nixStageClient
	(cd $(STAGING_ROOT) && zip -r - ./Estuary.jsexe/*) > estuary-client.zip

downloadDirtSamples:
	@ echo "downloadDirtSamples:"
	cd static && git clone https://github.com/TidalCycles/Dirt-Samples.git --depth 1
	-mkdir static/samples
	cd static/Dirt-Samples && cp -Rf * ../samples/
	rm -rf static/Dirt-Samples/
	@[ -d static/samples/bd ] || (echo "Error: make downloadDirtSamples did NOT work!" && exit 1)
	@ echo "Dirt samples downloaded."

makeSampleMap:
	@ echo "makeSampleMap:"
	@[ -d static/samples ] || (echo Directory static/samples does not exist. Have you provided a sample library, for example, by running 'make downloadDirtSamples'? && exit 1)
	@[ -f static/WebDirt/makeSampleMap.sh ] || (echo "Couldn't find static/WebDirt/makeSampleMap.sh - you probably have forgotten to 'git submodule update --init --recursive'" && exit 1)
	cd static/samples && bash ../WebDirt/makeSampleMap.sh . > sampleMap.json
	@[ -f static/samples/sampleMap.json ] || (echo "Error: make makeSampleMap did NOT work!" && exit 1)
	@ echo "Sample map made."

updateSubmodules:
	@ echo "updateSubModules:"
	git submodule update --init --recursive

fullBuild: downloadDirtSamples updateSubmodules makeSampleMap nixBuild cleanStage nixStageClient nixStageServer stageStaticAssets stageSamples

clean: cleanStage cleanDevStage
	-rm -rf result/
	-rm -rf dist-newstyle/
	-rm -rf dist-ghcjs/

runDevServer: STAGING_ROOT=$(DEV_STAGING_ROOT)
runDevServer: stageStaticAssets cabalBuildServer cabalStageServer
	cd ./$(STAGING_ROOT) && ./EstuaryServer

runServer: nixBuild stageStaticAssets stageSamples nixStageClient nixStageServer
	cd ./$(STAGING_ROOT) && ./EstuaryServer

selfCertificates:
	openssl genrsa -out staging/privkey.pem 2048
	openssl req -new -key staging/privkey.pem -out staging/cert.csr
	openssl x509 -req -in staging/cert.csr -signkey staging/privkey.pem -out staging/cert.pem
