SRCDIR = src
DOCEN = doc/en/
DOCDE = doc/de/
SUBTDIR = test


all: acl
doc: doc-de doc-en
install: install-acl
uninstall: uninstall-acl 
clean: clean-acl  clean-acltest clean-shared
install-doc:  install-doc-de  install-doc-en 
uninstall-doc: uninstall-doc-de uninstall-doc-en 
clean-doc: clean-doc-de clean-doc-en

###########################################################################
################################### ACL ###################################
###########################################################################
acl:
	$(MAKE) -C $(SRCDIR) all

install-acl:
	$(MAKE) -C $(SRCDIR) install	

uninstall-acl:
	$(MAKE) -C $(SRCDIR) uninstall	


clean-acl:
	 $(MAKE) -C $(SRCDIR) clean

###########################################################################
################################# SHARED ##################################
###########################################################################

shared:
	$(MAKE) -C $(SRCDIR) shared

install-shared:
	$(MAKE) -C $(SRCDIR) install-shared

unistall-shared:
	$(MAKE) -C $(SRCDIR) uninstall-shared


clean-shared:	
	$(MAKE) -C $(SRCDIR) clean-shared

###########################################################################
################################### TEST ##################################
###########################################################################

acltest:
	$(MAKE) -C $(SUBTDIR) all

clean-acltest:
	$(MAKE) -C $(SUBTDIR) clean


###########################################################################
############################## DOCUMENTATION ##############################
###########################################################################

############################## GERMAN ##############################

doc-de:
	$(MAKE) -C $(DOCDE) all
  
install-doc-de:
	$(MAKE) -C $(DOCDE) install	

unistall-doc-de:
	$(MAKE) -C $(DOCDE) uninstall	

clean-doc-de:
	$(MAKE) -C $(DOCDE) clean

############################## ENGLISH ##############################

doc-en:
	$(MAKE) -C $(DOCEN) all

install-doc-en:
	$(MAKE) -C $(DOCEN) install	

unistall-doc-en:
	$(MAKE) -C $(DOCEN) uninstall	

clean-doc-en:
	$(MAKE) -C $(DOCEN) clean
