# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=GTEDGE2v1 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to GTEDGE2v1 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "GTEDGE2v1 - Win32 Release" && "$(CFG)" !=\
 "GTEDGE2v1 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "GTEDGE2v1.mak" CFG="GTEDGE2v1 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GTEDGE2v1 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "GTEDGE2v1 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "GTEDGE2v1 - Win32 Debug"
F90=fl32.exe
RSC=rc.exe

!IF  "$(CFG)" == "GTEDGE2v1 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\GTEDGE2v1.exe"

CLEAN : 
	-@erase ".\Release\GTEDGE2v1.exe"
	-@erase ".\Release\transm.obj"
	-@erase ".\Release\Besj0.obj"
	-@erase ".\Release\Divrad.obj"
	-@erase ".\Release\Neutdist.obj"
	-@erase ".\Release\Interp.obj"
	-@erase ".\Release\Corad.obj"
	-@erase ".\Release\ncefits.obj"
	-@erase ".\Release\rotate.obj"
	-@erase ".\Release\sgesv_jm.obj"
	-@erase ".\Release\Neutfrac.obj"
	-@erase ".\Release\Divstab.obj"
	-@erase ".\Release\poloidal.obj"
	-@erase ".\Release\param.obj"
	-@erase ".\Release\CEFITS.OBJ"
	-@erase ".\Release\e0min.obj"
	-@erase ".\Release\Dsigmav.obj"
	-@erase ".\Release\Fuel.obj"
	-@erase ".\Release\Expint.obj"
	-@erase ".\Release\Radintg2.obj"
	-@erase ".\Release\Geometry.obj"
	-@erase ".\Release\Neutxpt.obj"
	-@erase ".\Release\SIGMAV.OBJ"
	-@erase ".\Release\Molecule.obj"
	-@erase ".\Release\Qdiv.obj"
	-@erase ".\Release\TOROTATE.OBJ"
	-@erase ".\Release\Radial.obj"
	-@erase ".\Release\EDGEROTRAN.OBJ"
	-@erase ".\Release\soldata.obj"
	-@erase ".\Release\Editdiv.obj"
	-@erase ".\Release\Disrupt.obj"
	-@erase ".\Release\Denlim.obj"
	-@erase ".\Release\lossfrac.obj"
	-@erase ".\Release\Soldiv.obj"
	-@erase ".\Release\width.obj"
	-@erase ".\Release\Neutral3.obj"
	-@erase ".\Release\DivSol.obj"
	-@erase ".\Release\ReflectION.obj"
	-@erase ".\Release\Svion.obj"
	-@erase ".\Release\Atomic.obj"
	-@erase ".\Release\cxrcefits.obj"
	-@erase ".\Release\Pedestal.obj"
	-@erase ".\Release\Limit2.obj"
	-@erase ".\Release\ATCOOL.OBJ"
	-@erase ".\Release\Distr.obj"
	-@erase ".\Release\divert3.obj"
	-@erase ".\Release\edgecalc.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /I "Release/" /c /nologo
F90_PROJ=/Ox /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/GTEDGE2v1.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/GTEDGE2v1.pdb" /machine:I386 /out:"$(OUTDIR)/GTEDGE2v1.exe" 
LINK32_OBJS= \
	"$(INTDIR)/transm.obj" \
	"$(INTDIR)/Besj0.obj" \
	"$(INTDIR)/Divrad.obj" \
	"$(INTDIR)/Neutdist.obj" \
	"$(INTDIR)/Interp.obj" \
	"$(INTDIR)/Corad.obj" \
	"$(INTDIR)/ncefits.obj" \
	"$(INTDIR)/rotate.obj" \
	"$(INTDIR)/sgesv_jm.obj" \
	"$(INTDIR)/Neutfrac.obj" \
	"$(INTDIR)/Divstab.obj" \
	"$(INTDIR)/poloidal.obj" \
	"$(INTDIR)/param.obj" \
	"$(INTDIR)/CEFITS.OBJ" \
	"$(INTDIR)/e0min.obj" \
	"$(INTDIR)/Dsigmav.obj" \
	"$(INTDIR)/Fuel.obj" \
	"$(INTDIR)/Expint.obj" \
	"$(INTDIR)/Radintg2.obj" \
	"$(INTDIR)/Geometry.obj" \
	"$(INTDIR)/Neutxpt.obj" \
	"$(INTDIR)/SIGMAV.OBJ" \
	"$(INTDIR)/Molecule.obj" \
	"$(INTDIR)/Qdiv.obj" \
	"$(INTDIR)/TOROTATE.OBJ" \
	"$(INTDIR)/Radial.obj" \
	"$(INTDIR)/EDGEROTRAN.OBJ" \
	"$(INTDIR)/soldata.obj" \
	"$(INTDIR)/Editdiv.obj" \
	"$(INTDIR)/Disrupt.obj" \
	"$(INTDIR)/Denlim.obj" \
	"$(INTDIR)/lossfrac.obj" \
	"$(INTDIR)/Soldiv.obj" \
	"$(INTDIR)/width.obj" \
	"$(INTDIR)/Neutral3.obj" \
	"$(INTDIR)/DivSol.obj" \
	"$(INTDIR)/ReflectION.obj" \
	"$(INTDIR)/Svion.obj" \
	"$(INTDIR)/Atomic.obj" \
	"$(INTDIR)/cxrcefits.obj" \
	"$(INTDIR)/Pedestal.obj" \
	"$(INTDIR)/Limit2.obj" \
	"$(INTDIR)/ATCOOL.OBJ" \
	"$(INTDIR)/Distr.obj" \
	"$(INTDIR)/divert3.obj" \
	"$(INTDIR)/edgecalc.obj"

"$(OUTDIR)\GTEDGE2v1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "GTEDGE2v1 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\GTEDGE2v1.exe"

CLEAN : 
	-@erase ".\Debug\GTEDGE2v1.exe"
	-@erase ".\Debug\lossfrac.obj"
	-@erase ".\Debug\ReflectION.obj"
	-@erase ".\Debug\soldata.obj"
	-@erase ".\Debug\Disrupt.obj"
	-@erase ".\Debug\transm.obj"
	-@erase ".\Debug\sgesv_jm.obj"
	-@erase ".\Debug\Neutfrac.obj"
	-@erase ".\Debug\Svion.obj"
	-@erase ".\Debug\Divrad.obj"
	-@erase ".\Debug\Interp.obj"
	-@erase ".\Debug\Fuel.obj"
	-@erase ".\Debug\ncefits.obj"
	-@erase ".\Debug\rotate.obj"
	-@erase ".\Debug\Qdiv.obj"
	-@erase ".\Debug\Geometry.obj"
	-@erase ".\Debug\cxrcefits.obj"
	-@erase ".\Debug\TOROTATE.OBJ"
	-@erase ".\Debug\CEFITS.OBJ"
	-@erase ".\Debug\Expint.obj"
	-@erase ".\Debug\SIGMAV.OBJ"
	-@erase ".\Debug\width.obj"
	-@erase ".\Debug\Besj0.obj"
	-@erase ".\Debug\Neutral3.obj"
	-@erase ".\Debug\Neutdist.obj"
	-@erase ".\Debug\Radial.obj"
	-@erase ".\Debug\Corad.obj"
	-@erase ".\Debug\Editdiv.obj"
	-@erase ".\Debug\Pedestal.obj"
	-@erase ".\Debug\Denlim.obj"
	-@erase ".\Debug\Soldiv.obj"
	-@erase ".\Debug\poloidal.obj"
	-@erase ".\Debug\Divstab.obj"
	-@erase ".\Debug\DivSol.obj"
	-@erase ".\Debug\param.obj"
	-@erase ".\Debug\Atomic.obj"
	-@erase ".\Debug\e0min.obj"
	-@erase ".\Debug\Distr.obj"
	-@erase ".\Debug\edgecalc.obj"
	-@erase ".\Debug\Dsigmav.obj"
	-@erase ".\Debug\Radintg2.obj"
	-@erase ".\Debug\Limit2.obj"
	-@erase ".\Debug\EDGEROTRAN.OBJ"
	-@erase ".\Debug\Molecule.obj"
	-@erase ".\Debug\Neutxpt.obj"
	-@erase ".\Debug\ATCOOL.OBJ"
	-@erase ".\Debug\divert3.obj"
	-@erase ".\Debug\GTEDGE2v1.ilk"
	-@erase ".\Debug\GTEDGE2v1.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /Zi /I "Debug/" /c /nologo
F90_PROJ=/Zi /I "Debug/" /c /nologo /Fo"Debug/" /Fd"Debug/GTEDGE2v1.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/GTEDGE2v1.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/GTEDGE2v1.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/GTEDGE2v1.exe" 
LINK32_OBJS= \
	"$(INTDIR)/lossfrac.obj" \
	"$(INTDIR)/ReflectION.obj" \
	"$(INTDIR)/soldata.obj" \
	"$(INTDIR)/Disrupt.obj" \
	"$(INTDIR)/transm.obj" \
	"$(INTDIR)/sgesv_jm.obj" \
	"$(INTDIR)/Neutfrac.obj" \
	"$(INTDIR)/Svion.obj" \
	"$(INTDIR)/Divrad.obj" \
	"$(INTDIR)/Interp.obj" \
	"$(INTDIR)/Fuel.obj" \
	"$(INTDIR)/ncefits.obj" \
	"$(INTDIR)/rotate.obj" \
	"$(INTDIR)/Qdiv.obj" \
	"$(INTDIR)/Geometry.obj" \
	"$(INTDIR)/cxrcefits.obj" \
	"$(INTDIR)/TOROTATE.OBJ" \
	"$(INTDIR)/CEFITS.OBJ" \
	"$(INTDIR)/Expint.obj" \
	"$(INTDIR)/SIGMAV.OBJ" \
	"$(INTDIR)/width.obj" \
	"$(INTDIR)/Besj0.obj" \
	"$(INTDIR)/Neutral3.obj" \
	"$(INTDIR)/Neutdist.obj" \
	"$(INTDIR)/Radial.obj" \
	"$(INTDIR)/Corad.obj" \
	"$(INTDIR)/Editdiv.obj" \
	"$(INTDIR)/Pedestal.obj" \
	"$(INTDIR)/Denlim.obj" \
	"$(INTDIR)/Soldiv.obj" \
	"$(INTDIR)/poloidal.obj" \
	"$(INTDIR)/Divstab.obj" \
	"$(INTDIR)/DivSol.obj" \
	"$(INTDIR)/param.obj" \
	"$(INTDIR)/Atomic.obj" \
	"$(INTDIR)/e0min.obj" \
	"$(INTDIR)/Distr.obj" \
	"$(INTDIR)/edgecalc.obj" \
	"$(INTDIR)/Dsigmav.obj" \
	"$(INTDIR)/Radintg2.obj" \
	"$(INTDIR)/Limit2.obj" \
	"$(INTDIR)/EDGEROTRAN.OBJ" \
	"$(INTDIR)/Molecule.obj" \
	"$(INTDIR)/Neutxpt.obj" \
	"$(INTDIR)/ATCOOL.OBJ" \
	"$(INTDIR)/divert3.obj"

"$(OUTDIR)\GTEDGE2v1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "GTEDGE2v1 - Win32 Release"
# Name "GTEDGE2v1 - Win32 Debug"

!IF  "$(CFG)" == "GTEDGE2v1 - Win32 Release"

!ELSEIF  "$(CFG)" == "GTEDGE2v1 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\width.for
DEP_F90_WIDTH=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\width.obj" : $(SOURCE) $(DEP_F90_WIDTH) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\transm.for
DEP_F90_TRANS=\
	".\geometry.fi"\
	

"$(INTDIR)\transm.obj" : $(SOURCE) $(DEP_F90_TRANS) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\TOROTATE.FOR
DEP_F90_TOROT=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\TOROTATE.OBJ" : $(SOURCE) $(DEP_F90_TOROT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Svion.for
DEP_F90_SVION=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Svion.obj" : $(SOURCE) $(DEP_F90_SVION) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Soldiv.for
DEP_F90_SOLDI=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Soldiv.obj" : $(SOURCE) $(DEP_F90_SOLDI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\SIGMAV.F

"$(INTDIR)\SIGMAV.OBJ" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\sgesv_jm.for

"$(INTDIR)\sgesv_jm.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\rotate.for
DEP_F90_ROTAT=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\rotate.obj" : $(SOURCE) $(DEP_F90_ROTAT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ReflectION.for
DEP_F90_REFLE=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\ReflectION.obj" : $(SOURCE) $(DEP_F90_REFLE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Radintg2.for
DEP_F90_RADIN=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Radintg2.obj" : $(SOURCE) $(DEP_F90_RADIN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Radial.for
DEP_F90_RADIA=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Radial.obj" : $(SOURCE) $(DEP_F90_RADIA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Qdiv.for
DEP_F90_QDIV_=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Qdiv.obj" : $(SOURCE) $(DEP_F90_QDIV_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\poloidal.for
DEP_F90_POLOI=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\poloidal.obj" : $(SOURCE) $(DEP_F90_POLOI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Pedestal.for
DEP_F90_PEDES=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Pedestal.obj" : $(SOURCE) $(DEP_F90_PEDES) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\param.for
DEP_F90_PARAM=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\param.obj" : $(SOURCE) $(DEP_F90_PARAM) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Neutxpt.FOR
DEP_F90_NEUTX=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Neutxpt.obj" : $(SOURCE) $(DEP_F90_NEUTX) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Neutral3.for
DEP_F90_NEUTR=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Neutral3.obj" : $(SOURCE) $(DEP_F90_NEUTR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Neutfrac.for
DEP_F90_NEUTF=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Neutfrac.obj" : $(SOURCE) $(DEP_F90_NEUTF) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Neutdist.for
DEP_F90_NEUTD=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Neutdist.obj" : $(SOURCE) $(DEP_F90_NEUTD) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ncefits.for

"$(INTDIR)\ncefits.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Molecule.for
DEP_F90_MOLEC=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Molecule.obj" : $(SOURCE) $(DEP_F90_MOLEC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\lossfrac.for
DEP_F90_LOSSF=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\lossfrac.obj" : $(SOURCE) $(DEP_F90_LOSSF) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Limit2.for
DEP_F90_LIMIT=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Limit2.obj" : $(SOURCE) $(DEP_F90_LIMIT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Interp.for
DEP_F90_INTER=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Interp.obj" : $(SOURCE) $(DEP_F90_INTER) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Geometry.for
DEP_F90_GEOME=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Geometry.obj" : $(SOURCE) $(DEP_F90_GEOME) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Fuel.for
DEP_F90_FUEL_=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Fuel.obj" : $(SOURCE) $(DEP_F90_FUEL_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Expint.for

"$(INTDIR)\Expint.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Editdiv.for
DEP_F90_EDITD=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Editdiv.obj" : $(SOURCE) $(DEP_F90_EDITD) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\EDGEROTRAN.FOR
DEP_F90_EDGER=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\EDGEROTRAN.OBJ" : $(SOURCE) $(DEP_F90_EDGER) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\edgecalc.for
DEP_F90_EDGEC=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\edgecalc.obj" : $(SOURCE) $(DEP_F90_EDGEC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\e0min.for
DEP_F90_E0MIN=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\e0min.obj" : $(SOURCE) $(DEP_F90_E0MIN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Dsigmav.f

"$(INTDIR)\Dsigmav.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Divstab.for
DEP_F90_DIVST=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Divstab.obj" : $(SOURCE) $(DEP_F90_DIVST) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\DivSol.for
DEP_F90_DIVSO=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\DivSol.obj" : $(SOURCE) $(DEP_F90_DIVSO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Divrad.for
DEP_F90_DIVRA=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Divrad.obj" : $(SOURCE) $(DEP_F90_DIVRA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\divert3.for
DEP_F90_DIVER=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\divert3.obj" : $(SOURCE) $(DEP_F90_DIVER) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Distr.for
DEP_F90_DISTR=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Distr.obj" : $(SOURCE) $(DEP_F90_DISTR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Disrupt.for
DEP_F90_DISRU=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Disrupt.obj" : $(SOURCE) $(DEP_F90_DISRU) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Denlim.for
DEP_F90_DENLI=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Denlim.obj" : $(SOURCE) $(DEP_F90_DENLI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\cxrcefits.for

"$(INTDIR)\cxrcefits.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Corad.for
DEP_F90_CORAD=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Corad.obj" : $(SOURCE) $(DEP_F90_CORAD) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\CEFITS.FOR

"$(INTDIR)\CEFITS.OBJ" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Besj0.for

"$(INTDIR)\Besj0.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Atomic.for
DEP_F90_ATOMI=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\Atomic.obj" : $(SOURCE) $(DEP_F90_ATOMI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ATCOOL.FOR
DEP_F90_ATCOO=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\ATCOOL.OBJ" : $(SOURCE) $(DEP_F90_ATCOO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\soldata.for
DEP_F90_SOLDA=\
	".\SOLDIV.FI"\
	

"$(INTDIR)\soldata.obj" : $(SOURCE) $(DEP_F90_SOLDA) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
