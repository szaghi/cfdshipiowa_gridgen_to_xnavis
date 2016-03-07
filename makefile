#!/usr/bin/make
#----------------------------------------------------------------------------------------------------------------------------------
# make init

# shell
SHELL = /bin/bash
# no verbose
$(VERBOSE).SILENT:
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# User options
COMPILER = gnu
DEBUG    = yes
F03STD   = yes
OPTIMIZE = no
OPENMP   = no
MPI      = no
TECIO    = no

.PHONY : DEFAULTRULE
DEFAULTRULE: $(DEXE)cfdshipiowa2xnavis

.PHONY : help
help:
	@echo
	@echo -e '\033[1;31m Make options of gridgen2mbinfo code\033[0m'
	@echo
	@echo -e '\033[1;31m Compiler choice\033[0m'
	@echo -e '\033[1;31m  COMPILER=gnu  \033[0m\033[1m => GNU gfortran          \033[0m'
	@echo -e '\033[1;31m  COMPILER=intel\033[0m\033[1m => Intel Fortran         \033[0m'
	@echo -e '\033[1;31m  COMPILER=pgi  \033[0m\033[1m => Portland Group Fortran\033[0m'
	@echo -e '\033[1;31m  COMPILER=g95  \033[0m\033[1m => free g95              \033[0m'
	@echo -e '\033[1;31m  COMPILER=$(COMPILER)  \033[0m\033[1m => default         \033[0m'
	@echo
	@echo -e '\033[1;31m Compiling options\033[0m'
	@echo -e '\033[1;31m  DEBUG=yes(no)   \033[0m\033[1m => on(off) debug                  (default $(DEBUG))\033[0m'
	@echo -e '\033[1;31m  F03STD=yes(no)  \033[0m\033[1m => on(off) check standard fortran (default $(F03STD))\033[0m'
	@echo -e '\033[1;31m  OPTIMIZE=yes(no)\033[0m\033[1m => on(off) optimization           (default $(OPTIMIZE))\033[0m'
	@echo -e '\033[1;31m  OPENMP=yes(no)  \033[0m\033[1m => on(off) OpenMP directives      (default $(OPENMP))\033[0m'
	@echo -e '\033[1;31m  MPI=yes(no)     \033[0m\033[1m => on(off) MPI    directives      (default $(MPI)) \033[0m'
	@echo
	@echo -e '\033[1;31m External libraries\033[0m'
	@echo -e '\033[1;31m  TECIO=yes(no)\033[0m\033[1m => on(off) Tecplot IO library linking (default $(TECIO))\033[0m'
	@echo
	@echo -e '\033[1;31m Provided Rules\033[0m'
	@echo -e '\033[1;31m  Defualt rule   =>\033[0m\033[1m gridgen2mbinfo\033[0m'
	@echo -e '\033[1;31m  help           =>\033[0m\033[1m printing this help message\033[0m'
	@echo -e '\033[1;31m  gridgen2mbinfo =>\033[0m\033[1m building gridgen2mbinfo code\033[0m'
	@echo -e '\033[1;31m  cleanobj       =>\033[0m\033[1m cleaning compiled object\033[0m'
	@echo -e '\033[1;31m  cleanmod       =>\033[0m\033[1m cleaning .mod files\033[0m'
	@echo -e '\033[1;31m  cleanmsg       =>\033[0m\033[1m cleaning make-log massage files\033[0m'
	@echo -e '\033[1;31m  cleanexe       =>\033[0m\033[1m cleaning executable files\033[0m'
	@echo -e '\033[1;31m  clean          =>\033[0m\033[1m running cleanobj, cleanmod and cleanmsg\033[0m'
	@echo -e '\033[1;31m  cleanall       =>\033[0m\033[1m running clean and cleanexe\033[0m'
	@echo -e '\033[1;31m  tar            =>\033[0m\033[1m creating a tar archive of the project\033[0m'
	@echo -e '\033[1;31m  doc            =>\033[0m\033[1m building the documentation\033[0m'
	@echo
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# directory & file
DSRC  = ./src/
DOBJ  = ./obj/
DMOD  = ./mod/
DLIB  = ./lib/
DEXE  = ./
VPATH = $(DSRC) $(DOBJ) $(DMOD) $(DLIB)

MKDIRS = $(DOBJ) $(DMOD) $(DEXE)
LBITS := $(shell getconf LONG_BIT)
ifeq "$(TECIO)" "yes"
  PREPROC = -DTECIO
	ifeq ($(LBITS),64)
  LIBS = $(DLIB)64bit/tecio64.a $(DLIB)64bit/libstdc++64.5.0.7.so
	else
		LIBS = $(DLIB)32bit/tecio.a $(DLIB)32bit/libstdc++.5.0.7.so
	endif
else
  PREPROC =
  LIBS =
endif
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# compiling and linking options
ifeq "$(COMPILER)" "gnu"
  OPTSC   = -cpp -c -J$(DMOD)
  OPTSL   =
  # debug
  ifeq "$(DEBUG)" "yes"
    PREPROC := $(PREPROC) -DDEBUG
    OPTSC := $(OPTSC) -O0 -Wall -Warray-bounds -fcheck=all -fbacktrace -ffpe-trap=invalid,overflow,underflow,precision,denormal
    OPTSL := $(OPTSL) -O0 -Wall -Warray-bounds -fcheck=all -fbacktrace -ffpe-trap=invalid,overflow,underflow,precision,denormal
#-Warray-temporaries
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -std=f2008 -fall-intrinsics
    OPTSL := $(OPTSL) -std=f2008 -fall-intrinsics
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O3
    OPTSL := $(OPTSL) -O3
  endif
  # openmp
  ifeq "$(OPENMP)" "yes"
    OPTSC := $(OPTSC) -fopenmp
    OPTSL := $(OPTSL) -fopenmp
    PREPROC := $(PREPROC) -DOPENMP
  endif
  # mpi
  ifeq "$(MPI)" "yes"
    PREPROC := $(PREPROC) -DMPI2
    FC = mpif90
  else
    FC = gfortran
  endif
endif
ifeq "$(COMPILER)" "intel"
  OPTSC   = -cpp -c -module $(DMOD)
  OPTSL   =
  # debug
  ifeq "$(DEBUG)" "yes"
    PREPROC := $(PREPROC) -DDEBUG
    CHK = -check all -check noarg_temp_created
    DEB = -debug all
    WRN = -warn all
    OPTSC := $(OPTSC) -O0 -fpe-all=0 -fp-stack-check -traceback $(WRN) $(CHK) $(DEB)
    OPTSL := $(OPTSL) -O0 -fpe-all=0 -fp-stack-check -traceback $(WRN) $(CHK) $(DEB)
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -std03
    OPTSL := $(OPTSL) -std03
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O3 -ipo
    OPTSL := $(OPTSL) -O3 -ipo
  endif
  # openmp
  ifeq "$(OPENMP)" "yes"
    OPTSC := $(OPTSC) -openmp
    OPTSL := $(OPTSL) -openmp
    PREPROC := $(PREPROC) -DOPENMP
  endif
  # mpi
  ifeq "$(MPI)" "yes"
    PREPROC := $(PREPROC) -DMPI2
    FC = mpif90
  else
    FC = ifort
  endif
endif
ifeq "$(COMPILER)" "pgi"
  OPTSC   = -Mpreprocess -c -module $(DMOD)
  OPTSL   =
	PREPROC := $(PREPROC) -Dpgf95
  # debug
  ifeq "$(DEBUG)" "yes"
    PREPROC := $(PREPROC) -DDEBUG
    OPTSC := $(OPTSC) -C -g -Mbounds -Mchkstk
    OPTSL := $(OPTSL) -C -g -Mbounds -Mchkstk
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -Mstandard
    OPTSL := $(OPTSL) -Mstandard
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O1
    OPTSL := $(OPTSL) -O1
  endif
  # openmp
  ifeq "$(OPENMP)" "yes"
    OPTSC := $(OPTSC) -mp
    OPTSL := $(OPTSL) -mp
    PREPROC := $(PREPROC) -DOPENMP
  endif
  # mpi
  ifeq "$(MPI)" "yes"
    PREPROC := $(PREPROC) -DMPI2
    FC = mpif90
  else
    FC = pgf95
  endif
endif
ifeq "$(COMPILER)" "g95"
  OPTSC   = -cpp -c -fmod=$(DMOD)
  OPTSL   =
  # debug
  ifeq "$(DEBUG)" "yes"
    PREPROC := $(PREPROC) -DDEBUG
    OPTSC := $(OPTSC) -O0 -g
    OPTSL := $(OPTSL) -O0 -g
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -std=f2003 -fintrinsic-extensions
    OPTSL := $(OPTSL) -std=f2003 -fintrinsic-extensions
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O3
    OPTSL := $(OPTSL) -O3
  endif
  FC = g95
endif
OPTSC := $(OPTSC) $(PREPROC)
OPTSL := $(OPTSL) $(PREPROC)

WHICHFC = $(shell which $(FC))

PRINTCHK = "\\033[1;31m Compiler used \\033[0m\\033[1m $(COMPILER) => $(WHICHFC)\\033[0m \n\
            \\033[1;31mSource dir    \\033[0m\\033[1m $(DSRC)\\033[0m \n\
            \\033[1;31mLibraries     \\033[0m\\033[1m $(LIBS)\\033[0m \n \n\
            \\033[1;31m Debug         \\033[0m\\033[1m $(DEBUG)\\033[0m \n\
            \\033[1;31m F-standard    \\033[0m\\033[1m $(F03STD)\\033[0m \n\
            \\033[1;31m Optimize      \\033[0m\\033[1m $(OPTIMIZE)\\033[0m \n\
            \\033[1;31m OpenMP        \\033[0m\\033[1m $(OPENMP)\\033[0m \n\
            \\033[1;31m MPI           \\033[0m\\033[1m $(MPI)\\033[0m \n\
            \\033[1;31m TECIO         \\033[0m\\033[1m $(TECIO)\\033[0m"
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
.PHONY : PRINTINFO
.NOTPARALLEL : PRINTINFO
PRINTINFO:
	@echo | tee make.log
	@echo -e $(PRINTCHK) | tee -a make.log
	@echo | tee -a make.log
	@echo -e '\033[1;31m Compiling options\033[0m' | tee -a make.log
	@echo -e '\033[1m [$(OPTSC)]\033[0m' | tee -a make.log
	@echo | tee -a make.log
	@echo -e '\033[1;31m Linking options \033[0m' | tee -a make.log
	@echo -e '\033[1m [$(OPTSL)]\033[0m' | tee -a make.log
	@echo | tee -a make.log

.PHONY : cleanobj
cleanobj:
	@echo -e "\033[1;31m deleting objects \033[0m" | tee make.log
	@rm -fr $(DOBJ)

.PHONY : cleanmod
cleanmod:
	@echo -e "\033[1;31m deleting mods \033[0m" | tee -a make.log
	@rm -fr $(DMOD)

.PHONY : cleanexe
cleanexe:
	@echo -e "\033[1;31m deleting exes \033[0m" | tee -a make.log
	@rm -f $(addprefix $(DEXE),$(EXES))

.PHONY : cleanmsg
cleanmsg:
	@rm -f diagnostic_messages
	@rm -f error_messages

.PHONY : clean
clean: cleanobj cleanmod cleanmsg

.PHONY : cleanall
cleanall: clean cleanexe

.PHONY : tar
tar: cleanall
	@echo -e "\033[1;31m Creating tar archive of the code \033[0m" | tee make.log
	@mkdir -p cfdshipiowa2xnavis
	@cp -rL lib src makefile cfdshipiowa2xnavis/
	@tar czf cfdshipiowa2xnavis.tgz cfdshipiowa2xnavis
	@rm -rf cfdshipiowa2xnavis

.PHONY : doc
doc:
	@echo -e "\033[1;31m Building documentation\033[0m" | tee make.log
	@doxygen .doxygenconfig

.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
#----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# rules of linking and compiling
COTEXT  = -e "\033[1;31m Compiling\033[0m\033[1m $(<F)\033[0m"
LITEXT  = -e "\033[1;31m Assembling\033[0m\033[1m $@\033[0m"
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))

$(DEXE)cfdshipiowa2xnavis : PRINTINFO $(MKDIRS) $(DOBJ)cfdshipiowa2xnavis.o
	@rm -f $(filter-out $(DOBJ)cfdshipiowa2xnavis.o,$(EXESOBJ))
	@echo | tee -a make.log
	@echo $(LITEXT) | tee -a make.log
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@ 1>> diagnostic_messages 2>> error_messages
EXES := $(EXES) cfdshipiowa2xnavis

$(DOBJ)cfdshipiowa2xnavis.o : cfdshipiowa2xnavis.f90 \
	$(DOBJ)ir_precision.o \
	$(DOBJ)data_type_os.o \
	$(DOBJ)data_type_tensor.o \
	$(DOBJ)data_type_vector.o \
	$(DOBJ)lib_io_misc.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)data_type_os.o : Data_Type_OS.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)data_type_tensor.o : Data_Type_Tensor.f90 \
	$(DOBJ)ir_precision.o \
	$(DOBJ)data_type_vector.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)data_type_vector.o : Data_Type_Vector.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)ir_precision.o : IR_Precision.f90
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_io_misc.o : Lib_IO_Misc.f90 \
	$(DOBJ)ir_precision.o \
	$(DOBJ)lib_strings.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_strings.o: ./src/Lib_Strings.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC) -I./src  $< -o $@
#-----------------------------------------------------------------------------------------------------------------------------------
