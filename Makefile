# The release branch should have a file named rebar.config.locked
use_locked_config = $(wildcard rebar.config.locked)
ifeq ($(use_locked_config),rebar.config.locked)
  rebar_config = rebar.config.locked
else
  rebar_config = rebar.config
endif
REBAR = $(shell pwd)/rebar -C $(rebar_config)

.PHONY: deps

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

rel: all
	$(REBAR) generate

test: compile
	${REBAR} eunit skip_deps=true

console:
	rel/alum/bin/alum console

relclean:
	rm -rf rel/alum

xref: all
	$(REBAR) skip_deps=true xref

update_locked_config:
	$(REBAR) lock-deps ignore=meck skip_deps=true

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/alum/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/alum/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/alum/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/alum/lib;)


##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel
DEVNODES ?= 4

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	  $(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev
