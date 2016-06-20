.PHONY: aap
.SUFFIXES:  .dvi .doc .txt .pl

FLUIT=flits
MACHINE:=$(shell gcc -dumpmachine)
ifeq ($(MACHINE), x86_64-linux-gnu)
PLATFORM=linux
PLBINDIR=$(HOME)/bin/
PL=swipl
XPCE=xpce
PLRT="/usr/local/swi/rt/bin/xpce"
XPCEBIN=$(PLBINDIR)$(XPCE)
PLBIN=$(PLBINDIR)$(PL)
PLLIBDIR:=$(shell eval `$(PLBIN) -dump-runtime-variables`;echo $$PLBASE)
SOLRTPLDIR:=$(shell dirname `dirname $(PLLIBDIR)`)
else
ifeq ($(MACHINE),i386-redhat-linux)
PLATFORM=linux
PL=pl
XPCE=xpce
PLRT="/usr/local/swi/rt/bin/xpce"
XPCEBIN=$(PLBINDIR)$(XPCE)
PLBIN=$(PLBINDIR)pl
PLLIBDIR:=$(shell eval `$(PLBIN) -dump-runtime-variables`;echo $$PLBASE)
SOLRTPLDIR:=$(shell dirname `dirname $(PLLIBDIR)`)
else
ifeq ($(MACHINE),sparc-sun-solaris2.8)
PLATFORM=solaris
PL=pl
XPCE=xpce
XPCEBIN=$(PLBINDIR)$(XPCE)
PLBIN=$(PLBINDIR)pl
PLLIBDIR:=$(shell eval `$(PLBIN) -dump-runtime-variables`;echo $$PLBASE)
SOLRTPLDIR:=$(shell dirname `dirname $(PLLIBDIR)`)
else
ifeq ($(MACHINE),i386-mingw32msvc)
PLATFORM=mingw32
PL="/program files/pl/bin/plcon"
PLRT=standalone
EXESUFFIX=.exe
else
ifeq ($(MACHINE),i686-pc-cygwin)
PLATFORM=cygwin
#PL="plcon"
PL=swipl-win
EXESUFFIX=.exe
#XPCE="plwin"
XPCE=swipl-win
else
ifeq ($(MACHINE),ppc-darwin)
PLATFORM=darwin
PL=swipl
XPCE=xpce
XPCEBIN=$(XPCE)
PLBIN=$(PL)
PLLIBDIR:=$(shell eval `$(PLBIN) -dump-runtime-variables`;echo $$PLBASE)
SOLRTPLDIR:=$(shell dirname `dirname $(PLLIBDIR)`)
SO=so
else
ifeq ($(MACHINE),i686-apple-darwin10)
PLATFORM=darwin10
PL=swipl
XPCE=swipl
XPCEBIN=$(XPCE)
PLBIN=$(PL)
PLLIBDIR:=$(shell eval `$(PLBIN) -dump-runtime-variables`;echo $$PLBASE)
SOLRTPLDIR:=$(shell dirname `dirname $(PLLIBDIR)`)
SO=so
else
ifeq ($(MACHINE),i686-apple-darwin11)
PLATFORM=darwin11
PL=swipl
XPCE=swipl
XPCEBIN=$(XPCE)
PLBIN=$(PL)
PLLIBDIR:=$(shell eval `$(PLBIN) -dump-runtime-variables`;echo $$PLBASE)
SOLRTPLDIR:=$(shell dirname `dirname $(PLLIBDIR)`)
SO=so
else
ifeq ($(findstring apple-darwin,$(MACHINE)), apple-darwin)
PLATFORM=darwin
PL=swipl
XPCE=swipl
XPCEBIN=$(XPCE)
PLBIN=$(PL)
PLLIBDIR:=$(shell eval `$(PLBIN) -dump-runtime-variables`;echo $$PLBASE)
SOLRTPLDIR:=$(shell dirname `dirname $(PLLIBDIR)`)
SO=so
else
PLATFORM=mingw32
PLDIR="/program files/pl/"
PLRT=standalone
PL="/program files/pl/bin/plcon"
endif
endif
endif
endif
endif
endif
endif
endif
endif

settings:
	@ echo PLATFORM:$(PLATFORM)
	@ echo "MACHINE: $(MACHINE)"
	@ echo "PL:      $(PL)"
	@ echo "XPCE:    $(XPCE)"

ifeq ($(PLATFORM),solaris)
TGZ=gtar -zcf
GTAR=gtar
else
TGZ=tar -zcf
GTAR=tar
endif

ppp:
	$(MAKE) $(BINFILES)

BINFILES=ltbare$(EXESUFFIX) \
	leadsto$(EXESUFFIX) \
	lteditor$(EXESUFFIX) \
	ttleditor$(EXESUFFIX) \
	ttlchecker$(EXESUFFIX) \
	log2trace$(EXESUFFIX) \
	abmptr$(EXESUFFIX)
INSTALLNOPREFFILES.nx=$(BINFILES:%=bin/%)
RTNEEDED=$(INSTALLFILES) $(LTINSTALLDIR)/plrt/bin/.chplflag $(RTPLNEEDED)

p:
	echo $(RTNEEDED)

instnew:
	$(MAKE) $(INSTALLFILES)
	$(MAKE) $(LTINSTALLDIR)/plrt/bin/.chplflag

rttgz:
	$(MAKE) LTINSTALLDIR=generated/lt  generated/lt/instrtok


generated/lt/instrtok:	$(RTNEEDED)
	echo INSTRTOK: $(RTNEEDED)
	$(MAKE) $(INSTALLFILES) $(RTNEEDED)
	cd generated;$(TGZ) leadstoinstallation.tgz lt

aap:
	echo AAP $(LTINSTALLDIR)/bin/leadsto
	$(MAKE) LTINSTALLDIR=generated/lt  $(EXECUTABLES)


$(LTINSTALLDIR)/plrt/bin/.chplflag:	$(RTPLNEEDED) $(EXECUTABLES)
	$(MAKE) $(EXECUTABLES)
	echo EX:  $(EXECUTABLES)
	echo RT: $(RTPLNEEDED)
	$(MAKE) $(RTPLNEEDED) $(EXECUTABLES)
	cd $(LTINSTALLDIR)/plrt/bin;./chplhd
	touch $@


$(LTINSTALLDIR)/plrt/bin/chplhd:	chplhd
	mkdir -p $(LTINSTALLDIR)/plrt/bin
	cp chplhd $@

RTPLNEEDEDLOC=xpce plrc chplhd
RTPLNEEDED=$(RTPLNEEDEDLOC:%=$(LTINSTALLDIR)/plrt/bin/%)


$(LTINSTALLDIR)/plrt/bin/xpce:	$(SOLRTPLDIR)/bin/xpce
	echo "MAKING xpce rt"
	echo $(LTINSTALLDIR)
	mkdir -p $(LTINSTALLDIR)/plrt/bin
	echo "NU XPCE"
	cp $(SOLRTPLDIR)/bin/xpce $(LTINSTALLDIR)/plrt/bin
	echo "DONE XPCE"
	cp $(PLLIBDIR)/swipl $(LTINSTALLDIR)/plrt
	eval `$(PLBIN) -dump-runtime-variables`;\
	mkdir -p $(LTINSTALLDIR)/plrt/lib/$$PLARCH;\
	for f in sgml2pl.so socket.so unix.so; \
	do \
		cp $(PLLIBDIR)/lib/$$PLARCH/$$f $(LTINSTALLDIR)/plrt/lib/$$PLARCH;\
	done;\
	cp $(PLLIBDIR)/xpce/lib/$$PLARCH/pl2xpce.so $(LTINSTALLDIR)/plrt/lib/$$PLARCH


$(LTINSTALLDIR)/plrt/bin/plrc:	$(SOLRTPLDIR)/bin/plrc
	mkdir -p $(LTINSTALLDIR)/plrt/bin
	cp $(SOLRTPLDIR)/bin/plrc $@
help:
	@ echo "commnsis (nsis + commdemo (only wcygwin))"
	@echo installai
	@echo installait
	@echo instpl
	@echo testai
	@echo test1all
	@ echo chkf:XPCE -f formframe.pl -g 'list_undefined'
	@echo savetraces.somepostfix : all tests in st/somepostfix/ directory

chkfv:
	$(XPCE) -f fv.pl -g '(list_undefined)'

tstfv:
	$(MAKE) fv$(EXESUFFIX);./fv -local -trace tsf.tr -run total_steam_failure_v6l.lt

tstfv1:
	$(MAKE) fv$(EXESUFFIX);./fv -local -test1

tstfv2:
	$(MAKE) leadsto$(EXESUFFIX);./leadsto -local -show_fv_trace spec/fvtest1.lt

tstfv3:
	$(MAKE) leadsto$(EXESUFFIX);./leadsto -local -show_fv_trace -displaytrace mhoo2.tr
tstfv4:
	$(MAKE) leadsto$(EXESUFFIX);./leadsto  -show_fv_trace -displaytrace mhoo.tr
tsttmp:
	$(MAKE) leadsto$(EXESUFFIX);./leadsto -local -show_fv_trace -displaytrace tmp.tr

sdemo:
	$(MAKE) clean
	if [ -f ../demo.zip ] ;\
	then \
		mv -f ../demo.zip ../demo1.zip;\
	fi
	rm -rf demo/
	cd ..;zip -r demo.zip pl
	mkdir -p demo
	cd demo;unzip ../../demo.zip
	chmod go-rwx ../demo.zip
	scp -p ../demo.zip lourens@$(FLUIT).cs.vu.nl:

dodemo:
	$(MAKE) leadsto$(EXESUFFIX);./leadsto -show_fv_trace -displaytrace mhoo2.tr

dodemo1:
	$(MAKE) fv$(EXESUFFIX);./fv -test1
ifeq ($(PLATFORM),cygwin)
abmptr$(EXESUFFIX):	abmptrace.pl abmpanalyse.pl abmputil.pl util.pl
	$(XPCE) -f abmpanalyse.pl -g 'abmpanalyse:savealone(tmp)'
	if chicon.exe tmp.exe tree.ico abmptr$(EXESUFFIX) ;\
	then true; else cp tmp$(EXESUFFIX) abmptr$(EXESUFFIX); fi
	rm tmp$(EXESUFFIX)
endif
tst0:
	$(XPCE) -f tocdat.pl -g tstdat

tstabmp1:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr
tstabmp2:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr swpc305/abmp/abmp1.dat
tstabmp3:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr swpc305/abmp/abmp2.dat
tstabmp4:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr -datfile swpc305/abmp/abmp2.dat
tstabmp5:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr -participant swpc305
tstabmp6:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr -participant swpc305 -participant swpc458

tstabmp7:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr -local -datfile vrobunew.dat
tstabmp8:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr -participant tst
tst9:
	$(MAKE) abmptr$(EXESUFFIX)
	./abmptr -local

tst10:
	$(MAKE) abmptr$(EXESUFFIX)
	rm -rf ../pl/home/
	tar -zxf abmp2004.tgz
	./abmptr -local -participants "'home/abmp04??'" -participants "'home/mcai??'"

fv$(EXESUFFIX):	fv.pl fv_config.pl util.pl trace_manager.pl
	$(XPCE) -f fv.pl -g 'savealone_fv(tmp)'
	if chicon.exe tmp.exe tree.ico fv$(EXESUFFIX) ;\
	then true; else cp tmp$(EXESUFFIX) fv$(EXESUFFIX); fi
	rm tmp$(EXESUFFIX)

abmptrtest:
	$(MAKE) ttlchecker.exe
	./ttlchecker -trace nego/traces/vu1009DAT1.tr -log all -checkq tibor3 -local nego/traces/negotiation12.fm

tst:
	$(MAKE) ltbare.exe;./ltbare.exe -local -log all spec/cwabug.lt

tec:
	$(XPCE) -f testcomment.pl -g test

EXPS=leadsto ltbare lteditor ttlchecker ttleditor log2trace
setlt1exp:
	cd /usr/local/ai/lt/bin;\
	for f in $(EXPS);\
	do \
		ls -l $$f.exp;\
		rm $$f.exp;\
		ln -s ../../lt1/bin/$$f $$f.exp;\
	done; \
	ls -l *.exp

setlt2exp:
	cd /usr/local/ai/lt/bin;\
	for f in $(EXPS);\
	do \
		ls -l $$f.exp;\
		rm $$f.exp;\
		ln -s ../../lt2/bin/$$f $$f.exp;\
	done; \
	ls -l *.exp

setlt3exp:
	cd /usr/local/ai/lt/bin;\
	for f in $(EXPS);\
	do \
		ls -l $$f.exp;\
		rm $$f.exp;\
		ln -s ../../lt3/bin/$$f $$f.exp;\
	done; \
	ls -l *.exp


h:
	$(MAKE) ttlchecker$(EXESUFFIX)
	./ttlchecker$(EXESUFFIX) -trace spec/test-trace.tr spec/ants-test.fm

tstd:
	$(MAKE) difftr$(EXESUFFIX)


difftr$(EXESUFFIX):	difftr.pl util.pl
	$(XPCE) -f difftr.pl -g 'save'



ts1:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -local spec/simple.lt



hla:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -noshow -graphview g1 -graphinit spec/highlevel2a.nl -displaytrace highlevel2.tr

$(LTINSTALLDIR)/bin/commdemo1:	Makefile
	echo $(LTINSTALLDIR)/bin/leadsto -graphview g1 -graphinit $(LTINSTALLDIR)/examples/test/highlevel2a.nl $(LTINSTALLDIR)/examples/test/highlevel2.lt > $@
	chmod +x $@
	chmod go+rX $@

le:
	$(MAKE) lteditor$(EXESUFFIX)
	./lteditor -local

mt:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -local -noshow model1.lt

hl:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -noshow -graphview g1 -graphinit spec/highlevel2a.nl -local -addterm 'display(ViewTag,show_atoms(_))' -displaytrace \
	highlevel2.tr

hl2:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -noshow -graphview g1  -local -displaytrace \
	dakota2.tr

pxor:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -local -pxor spec/pxor.lt

a:
	$(MAKE) ttlchecker
	./ttlchecker -trace sugarscape.tr -cholds  -local -checkq food_gone_all_traces spec/sugarscape1.fm


libplc1.so:	plc1.o
	g++ -shared -export-dynamic plc1.o -L/usr/lib/   -o libplc1.so


tc:	libplc1.so
	pl -f test.pl  -g tst

ts:
	$(XPCE) -f simtool.pl -g simtool:go

chks:
	$(XPCE) -f simtool.pl -g '(list_undefined)'

chkabmp:
	$(XPCE) -f abmpanalyse.pl -g '(list_undefined)'

tf:
	$(XPCE) -f formframe.pl -g lteditor

chkf:
	$(XPCE) -f formframe.pl -g 'list_undefined'

chktn:
	$(MAKE) $(SATSOURCES)
	$(XPCE) -f transnew.pl -g 'list_undefined'

ttn:
	$(MAKE) $(SATSOURCES)
	$(XPCE) -f transnew.pl -g 'transnew:test'

chkttl:
	$(MAKE) $(SATSOURCES)
	$(XPCE) -f ttlchecker.pl -g 'list_undefined,util:finalhalt(0)'

chk:
	$(XPCE) -f algo.pl -g 'list_undefined'

t1:
	$(MAKE) ttlchecker;./ttlchecker -trace spec/abmp.tr -batch_all abmp -checkq testpareto spec/abmp.fm

t2:
	$(MAKE) ttlchecker.exe;./ttlchecker -local -traces "'*DAT*.tr'"  -checkq testpareto spec/abmp.fm

t4:
	$(MAKE) ttlchecker.exe;./ttlchecker -local -pareto show_curves -trace vrobunewDAT1.tr  -checkq testpareto spec/abmp.fm

t3:
	$(MAKE) ttlchecker.exe;./ttlchecker -traces "'home/*.tr'"  -checkq testpareto -pareto show_curves spec/abmp.fm

t7:
	$(MAKE) ttlchecker.exe;./ttlchecker -local -traces home/mcai02DAT5ss.tr  -pareto show_curves spec/abmp.fm

#ABMPTR=abmp0308DAT1.tr
#ABMPTR=vu1003DAT1.tr
#ABMPTR=$(HOME)/wrk/abmp/vu10/ai02*.tr
#ABMPTR=$(HOME)/wrk/abmp/vu10/vu*.tr
ABMPTR=$(HOME)/wrk/abmp/vu10/abmp0311DAT1.tr
#ABMPTR=$(HOME)/wrk/abmp/vu10/abmp03*.tr
#ABMPTR=$(HOME)/wrk/abmp/vu10/abmp0309DAT1.tr
#ABMPTR=C:/cygwin/home/lourens/wrk/abmp/vu10/vu*.tr
#ABMPTR=$(HOME)/wrk/abmp/vu10/vu1009DAT1.tr
#ABMPTR=$(HOME)/wrk/abmp/vu10/vu1010DAT8.tr
#ABMPTR=$(HOME)/wrk/abmp/vu10/ai0218DAT1.tr
#ABMPTR=C:/cygwin/home/lourens/wrk/abmp/vu10/vu1010DAT8.tr
abmp:
	$(MAKE) ttlchecker$(EXESUFFIX)
	./ttlchecker -traces '$(ABMPTR)' -local spec/abmp.fm

m1:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -local model1.lt

m2:
	$(MAKE) ttlchecker$(EXESUFFIX)
	./ttlchecker -local -trace trace.tr model1.fm

test:
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto -local -modelchecking spec/heartn.lt
#	$(MAKE) ttlchecker;./ttlchecker -local -trace traceCom.tr -checkq test sugarscape.fm


#	xpce -f algo.pl -g "util:setlocal,algo:set_logging(all),readrunspec('machado.lt')"
#	xpce -f algo.pl -t  "set_logging(all),algo:readrunspec('machado.pl')"

TESTSPECS=spec1 spec2 simple forall1 forall2 heart
testfall:
	$(MAKE) ltbare
	for f in $(TESTSPECS);\
	do \
		./ltbare -nolog spec/$$f.lt;\
	done

test1all:
	$(MAKE) leadsto$(EXESUFFIX)
	for f in $(TESTSPECS); \
	do \
		./leadsto$(EXESUFFIX) -nolog spec/$$f.lt;\
	done

t0 :
	$(MAKE) ltbare$(EXESUFFIX)
	./ltbare$(EXESUFFIX) -local -logging all orgdyn.lt

test1.%:	spec/%.lt
	$(MAKE) leadsto$(EXESUFFIX)
	./leadsto$(EXESUFFIX) -local -logging all spec/$*.lt



test.%:	spec/%.lt
	xpce -f algo.pl -t  "algo:readrunspec('spec/$*.lt')"

ttrace:
	grep '[^(_a-zA-Z]trace[^_A-Za-z]' *.pl | grep -v \' | grep -v analyse.pl | grep -v trpl.pl | grep -v OK

chkt:
	$(XPCE) -f trpl.pl -g 'list_undefined,halt'

trpl:
	pl -f trpl.pl -t "trpl('../prolog.pl')"

trpl1:
	pl -f trpl.pl -g "trpl('../prolog.pl')"


LTSOURCES=algo.pl util.pl logutil.pl varutil.pl showtrace.pl formats.pl \
		psprinting.pl ltversion.pl modelchecking.pl rolegraph.pl \
#		fv.pl

STSOURCES=$(LTSOURCES) simtool.pl lt_config.pl

LTESOURCES=formframe.pl formats.pl formload.pl psprinting.pl util.pl \
	logutil.pl varutil.pl parteditors.pl externaleditor.pl \
	ltversion.pl nodes.pl form_config.pl
SATSOURCES=satgenut.pl satsimple.pl transsat.pl transnew.pl transutil.pl \
        satmacros.pl pareto.pl ttlchecker.pl transutil.pl transnew2.pl \
	satsimpleverbose.pl satsimplequiet.pl trace_manager.pl


version.nsi:	ltversion.pl
	$(PL) -f ltversion.pl -g "ltversion:save_version('version.nsi'),halt"

ifeq ($(PLATFORM),cygwin)


tstlog:
	$(MAKE) log2trace$(EXESUFFIX);./log2trace -L20m -G100m aap

chklog:
	$(XPCE) -f logtotrace.pl -g list_undefined

log2trace$(EXESUFFIX):	$(LTSOURCES) logtotrace.pl
	$(XPCE) -f logtotrace.pl -g 'logtotrace:savealone'



ltbare$(EXESUFFIX):	$(LTSOURCES)  tree.ico
	$(XPCE) -f algo.pl -g 'algo:savealone(tmp)'
	if chicon.exe tmp.exe tree.ico ltbare$(EXESUFFIX) ;\
	then true; else cp tmp.exe ltbare.exe; fi
	rm tmp.exe


leadsto$(EXESUFFIX):	$(STSOURCES)
	$(XPCE) -f simtool.pl -g 'simtool:savealone(tmp)'
	if chicon.exe tmp.exe tree.ico leadsto$(EXESUFFIX) ;\
	then true; else cp tmp.exe leadsto.exe; fi
	rm tmp.exe

lteditor$(EXESUFFIX):	$(LTESOURCES)
	$(XPCE) -f formframe.pl -g 'formframe:savealone(tmp)'
	if chicon.exe tmp.exe tree.ico lteditor$(EXESUFFIX) ;\
	then true; else cp tmp.exe lteditor.exe; fi
	rm tmp.exe

ttleditor$(EXESUFFIX):	$(LTESOURCES)
	$(XPCE) -f formframe.pl -g 'formframe:savealonefeditor(tmp)'
	if chicon.exe tmp.exe ttl.ico ttleditor$(EXESUFFIX) ;\
	then true; else cp tmp.exe ttleditor.exe; fi
	rm tmp.exe



ttlchecker$(EXESUFFIX):	$(LTESOURCES) $(SATSOURCES)
	$(XPCE) -f ttlchecker.pl -g 'ttlchecker:savealonettlchecker(tmp)'
	if chicon.exe tmp.exe ttl.ico ttlchecker$(EXESUFFIX) ;\
	then true; else cp tmp.exe ttlchecker.exe; fi
	rm tmp.exe

saveabmp$(EXESUFFIX):	util.pl savedata.pl logutil.pl ltversion.pl
	$(XPCE) -f savedata.pl -g 'savedata:compilesavedata(tmp)'
	if chicon.exe tmp.exe ttl.ico saveabmp$(EXESUFFIX) ;\
	then true; else cp tmp.exe saveabmp.exe; fi
	rm tmp.exe

rommel$(EXESUFFIX):	util.pl rommel.pl logutil.pl ltversion.pl
	$(XPCE) -f rommel.pl -g 'rommel:compilesavedata(tmp)'
	if chicon.exe tmp.exe ttl.ico rommel$(EXESUFFIX) ;\
	then true; else true; fi
	rm tmp.exe
tstr:
	$(MAKE) rommel$(EXESUFFIX)
	./rommel

endif


tstabmp:
	$(MAKE) saveabmp$(EXESUFFIX)
	./saveabmp -local


#NIEUWE SWIPL
ifneq ($(PLATFORM),cygwin)
ifeq ($(XPCE),swiplno)
STANDALONEOPT=--stand_alone=true
#STANDALONEOPT=
log2trace$(EXESUFFIX):	$(LTSOURCES) logtotrace.pl
	swipl -o logtotrace -g logtotrace:run $(STANDALONEOPT) -c logtotrace.pl


abmptr$(EXESUFFIX):	abmptrace.pl abmpanalyse.pl abmputil.pl util.pl
	swipl -o abmptr -g abmpanalyse:run $(STANDALONEOPT) -c abmpanalyse.pl



ltbare$(EXESUFFIX):	$(LTSOURCES)
	swipl -o ltbare -g algo:run $(STANDALONEOPT) -c algo.pl

leadsto$(EXESUFFIX):	$(STSOURCES)
	swipl -o leadsto -g simtool:go $(STANDALONEOPT) -c simtool.pl


lteditor$(EXESUFFIX):	$(LTESOURCES)
	swipl -o lteditor -g formframe:lteditor $(STANDALONEOPT) -c formframe.pl


ttleditor$(EXESUFFIX):	$(LTESOURCES)
	swipl -o ttleditor -g formframe:checkereditor $(STANDALONEOPT) -c formframe.pl


ttlchecker$(EXESUFFIX):	$(SATSOURCES) $(LTESOURCES)
	swipl -o ttlchecker -g ttlchecker:ttlchecker $(STANDALONEOPT) -c ttlchecker.pl
else
#No Save_Alone option
#NSA=_nsa
NSA=
#OLD pl
log2trace$(EXESUFFIX):	$(LTSOURCES) logtotrace.pl
	$(XPCE) -f logtotrace.pl -g 'logtotrace:savealone$(NSA)'

abmptr$(EXESUFFIX):	abmptrace.pl abmpanalyse.pl abmputil.pl util.pl
	$(XPCE) -f abmpanalyse.pl -g 'abmpanalyse:savealone$(NSA)(abmptr)'

ltbare$(EXESUFFIX):	$(LTSOURCES)
	$(XPCE) -f algo.pl -g 'algo:savealone$(NSA)(ltbare)'

leadsto$(EXESUFFIX):	$(STSOURCES)
	$(XPCE) -f simtool.pl -g 'simtool:savealone$(NSA)(leadsto)'


lteditor$(EXESUFFIX):	$(LTESOURCES)
	$(XPCE) -f formframe.pl -g 'formframe:savealone$(NSA)(lteditor)'

ttleditor$(EXESUFFIX):	$(LTESOURCES)
	$(XPCE) -f formframe.pl -g 'formframe:savealonefeditor$(NSA)(ttleditor)'




ttlchecker$(EXESUFFIX):	$(SATSOURCES) $(LTESOURCES)
	$(XPCE) -f ttlchecker.pl \
	-g 'ttlchecker:savealonettlchecker_nsa(ttlchecker)'
endif
endif


ifeq ($(PLATFORM),oldnotcygwin)
log2trace$(EXESUFFIX):	$(LTSOURCES) logtotrace.pl
	$(XPCE) -f logtotrace.pl -g 'logtotrace:savealone_nsa'

abmptr$(EXESUFFIX):	abmptrace.pl abmpanalyse.pl abmputil.pl util.pl
	$(XPCE) -f abmpanalyse.pl -g 'abmpanalyse:savealone_nsa(abmptr)'

ltbare$(EXESUFFIX):	$(LTSOURCES)
	$(XPCE) -f algo.pl -g 'algo:savealone_nsa(ltbare)'

leadsto$(EXESUFFIX):	$(STSOURCES)
	$(XPCE) -f simtool.pl -g 'simtool:savealone_nsa(leadsto)'


lteditor$(EXESUFFIX):	$(LTESOURCES)
	$(XPCE) -f formframe.pl -g 'formframe:savealone_nsa(lteditor)'

ttleditor$(EXESUFFIX):	$(LTESOURCES)
	$(XPCE) -f formframe.pl -g 'formframe:savealonefeditor_nsa(ttleditor)'




ttlchecker$(EXESUFFIX):	$(SATSOURCES) $(LTESOURCES)
	$(XPCE) -f ttlchecker.pl \
	-g 'ttlchecker:savealonettlchecker_nsa(ttlchecker)'
endif


satsimplequiet.pl:	satsimple.pl satsimplequiet1.pl
	cat satsimplequiet1.pl satsimple.pl > satsimplequiet.pl
satsimpleverbose.pl:	satsimple.pl satsimpleverbose1.pl
	cat satsimpleverbose1.pl satsimple.pl > satsimpleverbose.pl


pl:
	echo $(PLATFORM)

ifeq ($(PLATFORM),linux)
instpl:
	rm -f linux.tgz
	$(MAKE) instcommon.lt
	mkdir lt/pl
	tar -zcf linux.tgz lt/
	chmod go+r linux.tgz
	ls -l linux.tgz
	scp -p linux.tgz lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/
endif

ifeq ($(PLATFORM),linuxold)
instpl:
	rm -f linux.tgz
	$(MAKE) instcommon.lt
	mkdir lt/pl
	eval `pl -dump-runtime-variables`;cd $$PLBASE;tar cf - bin lib xpce xpce-*/lib |(cd /home/lourens/wrk/ww/db3/pl/lt/pl;tar xfBp -)
	echo "pl" > lt/swipl
	cp /usr/lib/libreadline.so.4.2 lt/pl/lib/
	cd lt/pl/lib;ln -s libreadline.so.4.2 libreadline.so.4
	tar -zcf ltrhlinux71.tgz lt/
	chmod go+r ltrhlinux71.tgz
	ls -l ltrhlinux71.tgz
	scp -p ltrhlinux71.tgz lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/
	chmod go-r ltrhlinux71.tgz
endif

ifeq ($(PLATFORM),darwin10)
instpl:
	rm -f darwin10.tgz
	$(MAKE) instcommon.lt
	mkdir lt/pl
	eval `swipl -dump-runtime-variables`;echo $$PLBASE;cp $$PLBASE/lib/*/libswipl.dylib lt/pl
#	eval `pl -dump-runtime-variables`;cd $$PLBASE;tar cf - bin lib xpce xpce-*/lib |(cd /home/lourens/wrk/ww/db3/pl/lt/pl;tar xfBp -)
#	echo "pl" > lt/swipl
#	cp /usr/lib/libreadline.so.4.2 lt/pl/lib/
	tar -zcf darwin10.tgz lt/
	chmod go+r darwin10.tgz
	ls -l darwin10.tgz
	scp -p darwin10.tgz lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/
#	chmod go-r ltrhlinux71.tgz

instpln:
	rm -rf lt
	mkdir -p lt/spec
	cp *.pl ChangeLog userman.html issues.html seedtree.gif tree.xpm lt
	- cp spec/* lt/spec
	rm -f lt/spec/highlevel2.lt
	cp Makefile.darwin lt/Makefile
	tar -zcf darwin10.tgz lt/
	chmod go+r darwin10.tgz
	ls -l darwin10.tgz
	#scp -p darwin10.tgz lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/

endif
ifeq ($(PLATFORM),darwin11)
instpl:
	rm -f darwin11.tgz
	$(MAKE) NSA=_nsa instcommon.lt
	mkdir lt/pl
	eval `swipl -dump-runtime-variables`;echo $$PLBASE;cd $$PLBASE/lib/i386*;cp *.dylib /Users/lourens/wrk/db3/pl/lt/pl
#	eval `pl -dump-runtime-variables`;cd $$PLBASE;tar cf - bin lib xpce xpce-*/lib |(cd /home/lourens/wrk/ww/db3/pl/lt/pl;tar xfBp -)
#	echo "pl" > lt/swipl
#	cp /usr/lib/libreadline.so.4.2 lt/pl/lib/
	tar -zcf darwin11.tgz lt/
	chmod go+r darwin11.tgz
	ls -l darwin11.tgz
#	scp -p darwin.tgz lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/
#	chmod go-r ltrhlinux71.tgz

endif

ifeq ($(PLATFORM),darwin)
instpl:
	rm -f darwin.tgz
	$(MAKE) NSA=_nsa instcommon.lt
#	mkdir lt/pl
#	eval `swipl -dump-runtime-variables`;echo $$PLBASE;cd $$PLBASE/lib/i386*;cp *.dylib /Users/lourens/wrk/db3/pl/lt/pl
#	eval `pl -dump-runtime-variables`;cd $$PLBASE;tar cf - bin lib xpce xpce-*/lib |(cd /home/lourens/wrk/ww/db3/pl/lt/pl;tar xfBp -)
#	echo "pl" > lt/swipl
#	cp /usr/lib/libreadline.so.4.2 lt/pl/lib/
	tar -zcf darwin.tgz lt/
	chmod go+r darwin.tgz
	ls -l darwin.tgz
#scp -p darwin11.tgz lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/
#	chmod go-r ltrhlinux71.tgz

endif




ty:
	$(MAKE) instsources
	cd ltpl/src;swipl -f makeleadsto.pl;ls -l ../bin
	cd ltpl/src;swipl -f makeeditors.pl;ls -l ../bin
	cd ltpl/src;swipl -f makettlchecker.pl;ls -l ../bin


instsources:
	rm -rf ltpl/*
	mkdir -p ltpl
	mkdir ltpl/bin
	mkdir -p ltpl/examples/test
	cd spec/;cp -r *.lt *.fm ../ltpl/examples/test/
	rm ltpl/examples/test/highlevel2.lt
#	rm ../$*/examples/test/highlevel2a.nl
#	cp README.win ltpl/README
	cp ChangeLog ltpl/ChangeLog
	cp userman.html issues.html seedtree.gif ltpl/
	mkdir -p ltpl/src
#	cp makeleadsto.pl makeeditors.pl makeltbare.pl makettlchecker.pl
	cp makeall.bat $(STSOURCES) $(SATSOURCES) $(LTESOURCES) pce_stuff.pl tree.xpm tree.ico \
	ltpl/src/
	zip -r ltpl.zip ltpl

instcommon.%:
	if [ "$*" != lt -a "$*" != ltpl ] ;\
	then \
		echo "illegal instcommon";exit 1;\
	fi
	rm -rf $*
	mkdir $*
	mkdir $*/bin
	mkdir -p $*/examples/test
	cd spec/;cp -r *.lt *.fm ../$*/examples/test/
	rm $*/examples/test/highlevel2.lt
#	rm ../$*/examples/test/highlevel2a.nl
#	cp README.win ltpl/README
	cp ChangeLog $*/ChangeLog
	cp userman.html issues.html seedtree.gif $*/
	$(MAKE) ltbare$(EXESUFFIX)
	$(MAKE) leadsto$(EXESUFFIX)
	$(MAKE) lteditor$(EXESUFFIX)
	$(MAKE) ttleditor$(EXESUFFIX)
	$(MAKE) ttlchecker$(EXESUFFIX)
	cp leadsto$(EXESUFFIX) $*/bin/
	cp ltbare$(EXESUFFIX) $*/bin/
	cp lteditor$(EXESUFFIX) $*/bin/
	cp ttleditor$(EXESUFFIX) $*/bin/
	cp ttlchecker$(EXESUFFIX) $*/bin/

ifeq  ($(PLATFORM),cygwin)

fvnsiss:
	$(MAKE) mhoo2.tr mhoo14.lt
	rm -f install_leadsto.exe
	$(MAKE) NSISOPT=/DCOMMDEMO NSISOPT2=/DABMP NSISOPT3=/DFV nsis
	mv install_leadsto.exe install_leadsto_fv.exe
	ls -l install_leadsto_fv.exe
	chmod go+rX install_leadsto_fv.exe
	scp -p install_leadsto_fv.exe lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/

commnsis:
	rm -f install_leadsto.exe
	$(MAKE) NSISOPT=/DCOMMDEMO NSISOPT2=/DABMP nsis
	mv install_leadsto.exe install_leadsto_comm.exe
	ls -l install_leadsto_comm.exe

nsis:
	$(MAKE) version.nsi
	$(MAKE) $(BINFILES)
	makensis $(NSISOPT) $(NSISOPT2) $(NSISOPT2) $(NSISOPT3) install.nsi

nsiss:
	$(MAKE) nsis
	chmod go+rX install_leadsto.exe
	scp -p install_leadsto.exe lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/install_leadsto_new.exe

tttt:
	$(PL) --help
scp:
	scp -p leadsto_`$(PL) -q -g '[ltversion],wrversion,halt' -f -t halt`.exe lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/


instpl:
	rm -f ltpl.zip
	$(MAKE) instcommon.ltpl
	cp tree.ico ltpl/bin/
	cd ltpl;unzip ../shortcuts.zip
	eval `plcon -dump-runtime-variables`;echo $$PLBASE;\
	cp "$$PLBASE/bin/libpl.dll" ltpl/bin;\
	cp "$$PLBASE/bin/pthreadVC.dll" ltpl/bin;\
	cp "$$PLBASE/bin/pl2xpce.dll" ltpl/bin;\
	cp "$$PLBASE/bin/plterm.dll" ltpl/bin/;\
	cp "$$PLBASE/bin/time.dll" ltpl/bin
	zip -r ltpl.zip ltpl
	chmod go+r ltpl.zip
	scp -p ltpl.zip lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/
	chmod go-r ltpl.zip
	ls -l ltpl.zip

instpls:
	$(MAKE) instpl
	scp -p ltpl.zip lourens@$(FLUIT).cs.vu.nl:www/aistaff/sim/
endif

EXECUTABLESLOC=leadsto ltbare lteditor ttleditor ttlchecker commdemo1 log2trace abmptr
EXAMPLESLOC=$(shell (cd spec;ls *.lt *.fm *.nl))
EXECUTABLES=$(EXECUTABLESLOC:%=$(LTINSTALLDIR)/bin/%)
EXAMPLES=$(EXAMPLESLOC:%=$(LTINSTALLDIR)/examples/test/%)
EXAMPLESSPEC=$(EXAMPLESLOC:%=spec/%)
INSTALLFILES=$(EXECUTABLES) $(EXAMPLES) $(OTHERS)
OTHERSLOC=userman.html issues.html seedtree.gif ChangeLog \
getting_started_LEADSTO.pdf getting_started_TTL.pdf
OTHERS=$(OTHERSLOC:%=$(LTINSTALLDIR)/%)

$(LTINSTALLDIR)/examples/test/%.nl:	spec/%.nl
	mkdir -p $(LTINSTALLDIR)/examples/test
	chmod go+rX $(LTINSTALLDIR)/examples/test
	cp  $? $(LTINSTALLDIR)/examples/test/
	chmod go+rX $@

$(LTINSTALLDIR)/examples/test/%.fm:	spec/%.fm
	mkdir -p $(LTINSTALLDIR)/examples/test
	chmod go+rX $(LTINSTALLDIR)/examples/test
	cp  $? $(LTINSTALLDIR)/examples/test/
	chmod go+rX $@

$(LTINSTALLDIR)/examples/test/%.lt:	spec/%.lt
	mkdir -p $(LTINSTALLDIR)/examples/test
	chmod go+rX $(LTINSTALLDIR)/examples/test
	cp  $? $(LTINSTALLDIR)/examples/test/
	chmod go+rX $@



#ugly too much copy
$(LTINSTALLDIR)/bin/log2trace:    log2trace
	mkdir -p $(LTINSTALLDIR)/bin
	cp log2trace $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin/log2trace

$(LTINSTALLDIR)/bin/abmptr:    abmptr
	mkdir -p $(LTINSTALLDIR)/bin
	cp abmptr $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin/abmptr



$(LTINSTALLDIR)/bin/leadsto:	leadsto
	mkdir -p $(LTINSTALLDIR)/bin
	cp leadsto $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin/leadsto

$(LTINSTALLDIR)/bin/ltbare:	ltbare
	mkdir -p $(LTINSTALLDIR)/bin
	cp ltbare $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin/ltbare

$(LTINSTALLDIR)/bin/lteditor:	lteditor
	mkdir -p $(LTINSTALLDIR)/bin
	cp lteditor $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin/lteditor

$(LTINSTALLDIR)/bin/ttleditor:	ttleditor
	mkdir -p $(LTINSTALLDIR)/bin
	cp ttleditor $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin/ttleditor

$(LTINSTALLDIR)/bin/ttlchecker:	ttlchecker
	mkdir -p $(LTINSTALLDIR)/bin
	cp ttlchecker $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin
	chmod go+rX $(LTINSTALLDIR)/bin/ttlchecker



#ugly too much copy
$(OTHERS):	$(OTHERSLOC)
	mkdir -p $(LTINSTALLDIR)
	cp $(OTHERSLOC) $(LTINSTALLDIR)
	chmod go+rX $(LTINSTALLDIR)

installai:	$(INSTALLFILES)

HTMLFILES=userman.html issues.html seedtree.gif ChangeLog
installait:
	cd /usr/local/ai/pc/projects;unzip -o /home/lourens/www/aistaff/sim/ltpl.zip
	cp /usr/local/ai/pc/projects/ltpl/shortcuts/setupltVUwindows.bat /usr/local/ai/pc/projects/setupltsim.bat
	cp /usr/local/ai/pc/projects/ltpl/shortcuts/setupltVUwindows.bat /usr/local/ai/pc/setupltsim.bat
	chmod go+rX /usr/local/ai/pc/projects/setupltsim.bat
	chmod -R go+rX /usr/local/ai/pc/projects/ltpl/
	cp $(HTMLFILES) /home/lourens/www/aistaff/sim/
	cd /home/lourens/www/aistaff/sim/;chmod go+r $(HTMLFILES)
	cp /home/lourens/www/aistaff/sim/ltpl.zip /home/lourens/www/sim/
	- cp /home/lourens/www/aistaff/sim/install_leadsto.exe /home/lourens/www/sim/
	- chmod go+rX /home/lourens/www/sim/install_leadsto.exe
	chmod go+r /home/lourens/www/sim/ltpl.zip
	cp $(HTMLFILES) /home/lourens/www/sim/
	cd /home/lourens/www/sim/;chmod go+r $(HTMLFILES)
	cp $(HTMLFILES) /home/lourens/www/
	cd /home/lourens/www/;chmod go+r $(HTMLFILES)
	cp /home/lourens/www/aistaff/sim/ltrhlinux71.tgz /home/lourens/www/sim/
	chmod go+rX /home/lourens/www/sim/ltrhlinux71.tgz

testai:
	$(LTINSTALLDIR)/bin/leadsto $(LTINSTALLDIR)/examples/test/simple.lt

cleanall:
	$(MAKE) clean
	rm -rf ../pl/home/
	rm -rf generated

%.cleantex:
	rm -f $*.aux $*.idx $*.ilg $*.ind $*.out $*.toc

clean:
	$(MAKE) leadsto.cleantex
	rm -f *~ scratch.eps trace.tr leadsto$(EXESUFFIX) ltbare$(EXESUFFIX) lteditor$(EXESUFFIX) abmptr$(EXESUFFIX) fv$(EXESUFFIX)
	rm -f ttleditor$(EXESUFFIX) ttlchecker$(EXESUFFIX) log2trace$(EXESUFFIX)
	rm -f  log ltpl.zip ltrhlinux71.tgz
	rm -rf lt ltpl satsimpleverbose.pl satsimplequiet.pl
	rm -f generatedholds.pl generated_compacted_trace_*.tr
	rm -f interact-a.tr interact-b.tr
	rm -f install_leadsto.exe install_leadsto_comm.exe
	rm -f abmptr.exe fv.exe saveabmp.exe
	rm -f spec/*~
	rm -f compiled.txt version.nsi log.log
	rm -f darwin.tgz typescript
	rm -rf shortcuts

chktr:
	$(XPCE) -f formframe.pl -g 'list_undefined'

tr:
	xpce -f formframe.pl -g 'formframe:lteditor'

TR1=norecwait
TR2=recwait
#TR2=tb085
difftr1:
	@ echo diffing traces st/$(TR1) st/$(TR2)
	@ rm -f difflog samelog
	@cd st/$(TR1);\
	for f in *.tr; \
	do \
		echo FILE:$$f;\
		if diff -B $$f ../$(TR2)/$$f | grep -v "content(run" | grep -v "content(gene" | grep -v 2c2 | grep -v 4c4 | grep -v '[-]--' ;\
		then \
			echo $$f >> difflog;\
		else \
			echo $$f >> samelog;\
		fi \
	done

#TESTS=spec/simple spec/heart spec/forall1 spec/forall2 spec/spec1 spec/spec2
# cell
TESTDIR=spec
TESTS=$(shell cat all)
#TESTS=$(shell cd $(TESTDIR);find . -type f -print)
TESTS=heart.lt simple.lt machado.lt


au:
	echo $(TESTS)

TESTOPTIONS=
testtraces:
	$(MAKE) ltbare$(EXESUFFIX)
	rm -f yes no
	@ulimit -t 30 ;echo "SET ulimit to 30";for f in $(TESTS);\
	do\
		if [ -f stop ] ; \
		then \
			echo stop;\
			exit 1;\
		fi; \
		echo "**TEST $$TESTDIR/$$f";\
		if ./ltbare$(EXESUFFIX) $(TESTOPTIONS) -nologging -noshow $(TESTDIR)/$$f;\
		then \
			echo $$f >> yes;\
		else \
			echo $$f >> no;\
		fi;\
	done

tt:
	$(MAKE) ltbare$(EXESUFFIX)
	time $(SAVELT) -local -nologging -savetrace st/$*/$$f.tr -noshow spec/heart.lt

#SAVELT=./ltbare$(EXESUFFIX)
SAVELT=./ltbare$(EXESUFFIX)
savetraces.%:
	$(MAKE) ltbare$(EXESUFFIX)
	@ if test -x st/$* ;\
	then \
		echo "ERROR:savetrace directory \"st/$*\" exists.";\
		exit 1;\
	fi
	@mkdir -p st/$*
	@echo "make savetraces.$* results" > st/$*/log
	@echo "      LTBARE:$(SAVELT)" >> st/$*/log
	@uname -a >> st/$*/log
	@date >> st/$*/log
	@ for f in $(TESTS);\
	do\
		echo "**TEST $$TESTDIR/$$f";\
		echo "**TEST $$TESTDIR/$$f" >> st/$*/times;\
		echo "******************" >> st/$*/log;\
		ls -l $(TESTDIR)/$$f >> st/$*/log;\
		(time $(SAVELT) -nologging -savetrace st/$*/$$f.tr -noshow $(TESTDIR)/$$f) >> st/$*/log 2>> st/$*/times;\
		echo "**END TEST $$f" >> st/$*/times;\
	done
	ls -l st/$*


TIBORSOURCES=interact-a.tr interact-b.tr spec/templ.fm

interact-b.tr:	spec/interact5.lt
	$(MAKE) ltbare$(EXESUFFIX)
	./ltbare -noshow -nolog -savetrace interact-b.tr spec/interact5.lt
interact-a.tr:	spec/interact5.lt
	$(MAKE) ltbare$(EXESUFFIX)
	./ltbare -noshow -nolog -constant dsetup=5 -savetrace interact-a.tr spec/interact5.lt


tibortests=gp1 gp2 gp3 gp4 m1 m2 m3 m4 s1_back s2_r_back p1_1_back \
p1_2_back p1_3_back s1_for s2_r_for p1_1_for p1_2_for p1_3_for p2_for \
c_for c_for_old p1_2_back2_LOURENS s2_r_for_LOURENS p2_back c_back
tiborteststimeout=p1_2_back2


ttt:
	$(MAKE) ttlchecker interact-a.tr
	./ttlchecker -nogui -nolog -checkq  -trace interact-a.tr spec/templ.fm
tibortest:
	@ $(MAKE) $(TIBORSOURCES) ttlchecker
	@ if [ ! -d tibortestdir ] ;\
	then \
		mkdir tibortestdir;\
	fi
	@ if test -f tibortestdir/cnt; \
	then \
		nr=`cat tibortestdir/cnt`;\
		rm tibortestdir/cnt;\
	else \
		nr=0;\
	fi;\
	echo test to tibortestdir/log$$nr;\
	expr $$nr + 1 > tibortestdir/cnt;\
	uname -a > tibortestdir/log;\
	 (./ttlchecker -nolog -log version_header \
		-log version_details -nogui) >> tibortestdir/log 2>&1;\
	date >> tibortestdir/log;\
	echo "TIBORTEST" >> tibortestdir/log;\
	(\
	for f in $(tibortests);\
	do \
		echo >> tibortestdir/log;\
		echo >> tibortestdir/log;\
		echo $$f >> tibortestdir/log;\
		(time ./ttlchecker -nogui -nolog -checkq $$f -trace interact-a.tr -trace interact-b.tr -nolog -log timing -nogui spec/templ.fm) >>  tibortestdir/log 2>&1 ;\
	done;\
	echo "TOTAL TIME:" >> tibortestdir/log) > tibortestdir/tmp 2>&1;\
	cat tibortestdir/log tibortestdir/tmp > tibortestdir/log$$nr
	grep Formula tibortestdir/log$$nr
#	cat tibortestdir/log$$nr

tbt:
	expr 1 + 1


exportlt:
	$(MAKE) cleanall
	mkdir -p generated/lt
	cp -r *.pl *.xpm *.html ChangeLog Makefile* *.txt *.ico DEV README README.win TODO algo chplhd *.doc *.c *.cc ds.h install.nsi leadsto.pd nego options.ini pars seedtree.gif shortcuts.zip spec total_steam_failure_v6l.lt generated/lt
	rm -f LT.zip
	cd generated;\
	zip -r LT.zip lt
	ls -l generated/LT.zip


mkgithubissues:
	$(MAKE) githubissues0 githubfilterissues

githubissues0:
	curl https://api.github.com/repos/lourensm/leadsto/issues?state=all > tmpissues

githubfilterissues:
	grep -v '"login": "lourensm"' tmpissues | grep -v '"repository_url": "https://api.github.com/repos/lourensm/leadsto",' | grep -v '"url": "https://api.github.com/users/lourensm",' | grep -v '"avatar_url": "https://avatars.githubusercontent.com/u/2205402?v=3",' | grep -v '"followers_url": "https://api.github.com/users/lourensm/followers",' | grep -v '"following_url": "https://api.github.com/users/lourensm/following{/other_user}",' | grep -v '"gists_url": "https://api.github.com/users/lourensm/gists{/gist_id}",' | grep -v '"starred_url": "https://api.github.com/users/lourensm/starred{/owner}{/repo}",' | grep -v '"subscriptions_url": "https://api.github.com/users/lourensm/subscriptions",' | grep -v '"organizations_url": "https://api.github.com/users/lourensm/orgs",' | grep -v '"html_url": "https://github.com/lourensm",' | grep -v '"repos_url": "https://api.github.com/users/lourensm/repos",' | grep -v '"events_url": "https://api.github.com/users/lourensm/events{/privacy}",' | grep -v '"received_events_url": "https://api.github.com/users/lourensm/received_events",' | grep -v '    "url": "https://api.github.com/repos/lourensm/leadsto/issues/' | grep -v '    "labels_url": "https://api.github.com/repos/lourensm/leadsto/issues/' | grep -v '    "comments_url": "https://api.github.com/repos/lourensm/leadsto/issues' | grep -v '    "events_url": "https://api.github.com/repos/lourensm/leadsto/issues' | grep -v '    "html_url": "https://github.com/lourensm/leadsto/issues' | grep -v '    "closed_at": null,' | grep -v '      "id": 2205402,' | grep -v '      "gravatar_id": "",' | grep -v '      "type": "User",' | grep -v '    "comments": 0,' | grep -v '      "site_admin": false' | grep -v '    "assignee": null,' | grep -v '    "locked": false,' | grep -v '    "milestone": null,' > githubissues

PLMANDIR=$(HOME)/software/swipl-devel/man
DOC2TEX=$(PLMANDIR)/doc2tex

DOC=leadsto
TEX=$(DOC).tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
HTML=$(DOC).html

pdf:
	$(MAKE) $(DOC).pdf

$(HTML):	$(TEX)
		latex2html $(DOC)
		mv pldoc.html leadsto.html

$(PDF):		$(TEX)
		$(PLMANDIR)/runtex --pdf $(DOC)

$(TEX):		$(DOC2TEX)

leadsto.tex:	leadsto.doc
	$(DOC2TEX) leadsto.doc > leadsto.tex

doc:
	$(DOC2TEX) leadsto.doc > leadsto.tex


# gathered examples in spec1 directory, now test diif with spec directory

diffspec:
	cd spec;\
	for f in *;\
		do \
			echo $$f;\
			if [ -f ../spec1/$$f ] ;\
			then\
				echo "In spec1";\
				diff $$f ../spec1/$$f;\
				rm ../spec1/$$f;\
			else \
				echo "NOT";\
			fi \
		done

ltall:
	cd spec1;\
	for f in *.lt;\
		do \
			echo "xxxxxxxxxxxxxxxxxxxxxxxxx$$f xxxxxxxxxxxxxxxxx";\
			../leadsto -noshow -nologging $$f | grep -v "Specified no show" | grep -v "Option -noshow => no GUI" | grep -v "SAVED TRACE IN trace.tr" ;\
		done


debug1:
	$(MAKE) leadsto
	./leadsto -debug1 -log all spec1/bugje-nondeterminism.lt

stopbug:
	$(MAKE) leadsto
	rm -f randomstate.txt
	counter=0
	while true; do \
		if [ -e randomstate.txt ] ;\
		then \
			echo "FOUND rs";\
			exit 1;\
		fi;\
		let counter=counter+1;\
		./leadsto -debug1 -noshow -log all spec1/bugje-nondeterminism.lt;\
		echo "COUNTER:" $$counter;\
	done
