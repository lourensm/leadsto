; install.nsi
;TODO:
;Add version to install executable name, also to 
;install text
;Probably best from within make:
;first identify ltversion
;then call makensis /DVERSION=`getversion`
;add shortcut for .tr files
;do not allow choice of extensions
;Only overwrite existing entry after confirmation
;Provide installation not doing anything with registry
;Cleanup old shortcuts if new ones not specified.
;.fm .lt .tr
;examples in working directory
;CLEANUP of old install directory if not same as new one?
;ASK for confirmation of removal of CWD in case of it being part of
;   INSTDIR etc

!include "version.nsi"
!define TEMP1 $R0 ; Temp variable
!define CWD    $4 ; Working directory for startmenu shortcut
!define DEFCWD $6 ; Default working directory


;
;--------------------------------
;    !define PLLIB "C:\Program Files\pl\bin"
; The name of the installer
; (generated into version.nsi)Name "Leadsto & TTL Checking software VERSION ${VERSION}"

ReserveFile "${NSISDIR}\Plugins\UserInfo.dll"
ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
ReserveFile "options.ini"


; The file to write
OutFile "install_leadsto.exe"

; The default installation directory
InstallDir $PROGRAMFILES\ltpl1

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM SOFTWARE\LT_PL1 "Install_Dir"

; The text to prompt the user to enter a directory
ComponentText "This will install the leadsto software ${VERSION} on your computer. $\nSelect which optional things you want installed."

; The text to prompt the user to enter a directory
DirText "Choose a directory to install the Leadsto/Ttl Checker software into:"

;--------------------------------
LicenseData "license.txt"
LicenseText "The Leadsto/Ttl Checker software is NOT public domain software"
Page license
Page components
Page directory
Page custom SetCustom "" ": Installation options"
Page instfiles

; The stuff to install
Section "Base System (required)"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there
  File "license.txt"
  SetOutPath $INSTDIR\bin
  FILE ltbare.exe
  FILE leadsto.exe
  FILE lteditor.exe
  FILE ttleditor.exe
  FILE ttlchecker.exe
!ifdef ABMP
  FILE log2trace.exe
  FILE abmptr.exe
!endif
  FILE tree.ico
  FILE ttl.ico

  FILE "${PLLIB}\*.dll"
;  FILE "${PLLIB}\libgmp-10.dll"
;  FILE "${PLLIB}\pthreadGC2.dll"
;  FILE "${PLLIB}\plterm.dll"
;  FILE "${PLLIB}\pl2xpce.dll"



;  FILE "${PLLIB}\libpl.dll"
;  FILE "${PLLIB}\pthreadVC.dll"
;  FILE "${PLLIB}\pl2xpce.dll"
;  FILE "${PLLIB}\plterm.dll"
;  FILE "${PLLIB}\time.dll" 
  CreateDirectory ${CWD}\examples

  SetOutPath ${CWD}\examples
!ifdef COMMDEMO
  FILE "spec\highlevel2a.nl"
  FILE "spec\highlevel2.lt"
!endif
!ifdef FV
  FILE "mhoo2.tr"
  FILE "mhoo14.lt"
  FILE "spec\fvtest0.lt"
  FILE "spec\fvtest1.lt"
!endif
  FILE "spec\spec1.lt"
  FILE "spec\spec1.tr"
  FILE "spec\example1.fm"
  FILE "spec\example2.fm"
  FILE "spec\example3.fm"
  FILE "spec\spec1check.fm"
  FILE "spec\cell.lt"
  FILE "spec\cell2.lt"
  FILE spec\ants-test.fm
  FILE spec\ants22.lt
  FILE spec\expfn.lt
  FILE spec\heart.lt
  FILE spec\heart_agr.lt
  FILE spec\heartn.lt
  FILE spec\model1.fm
  FILE spec\model1.lt
  FILE spec\orgdyn_coll9.lt
  FILE spec\simple.fm
  FILE spec\simple.lt
  FILE spec\sugarscape.lt
  FILE spec\sugarscape1.fm
  FILE spec\test-trace.tr
  FILE spec\test_external.fm
  FILE spec\testdisplay.lt
  FILE spec\testvars.lt
!ifdef ABMP
  FILE spec\abmp.fm
  FILE spec\abmp.tr
!endif
  CreateDirectory $INSTDIR\shortcuts
  CreateShortCut "$INSTDIR\shortcuts\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0

;  SetOutPath $INSTDIR\examples
SetOutPath "${CWD}"
  CreateShortCut "$INSTDIR\shortcuts\leadsto.lnk" "$INSTDIR\bin\leadsto.exe" "" "$INSTDIR\bin\tree.ico" 0
  CreateShortCut "$INSTDIR\shortcuts\leadstowide.lnk" "$INSTDIR\bin\leadsto.exe" "-tracerangepixels 450" "$INSTDIR\bin\tree.ico" 0
  CreateShortCut "$INSTDIR\shortcuts\ttlchecker.lnk" "$INSTDIR\bin\ttlchecker.exe" "" "$INSTDIR\bin\tree.ico" 0
  CreateShortCut "$INSTDIR\shortcuts\lteditor.lnk" "$INSTDIR\bin\lteditor.exe" "" "$INSTDIR\bin\tree.ico" 0
  CreateShortCut "$INSTDIR\shortcuts\usermanual.lnk" "$INSTDIR\userman.html"
!ifdef FV
SetOutPath "${CWD}\examples"
  CreateShortCut "$INSTDIR\shortcuts\fv-demo.lnk" "$INSTDIR\bin\leadsto.exe" "-tracerangepixels 450 -show_fv_trace -displaytrace mhoo2.tr" "$INSTDIR\bin\tree.ico" 0
SetOutPath "${CWD}"
!endif
SetOutPath "${CWD}\examples"
!ifdef COMMDEMO
  CreateShortCut "$INSTDIR\shortcuts\commdemo1.lnk" "$INSTDIR\bin\leadsto.exe" "-graphview g1 -graphinit highlevel2a.nl highlevel2.lt" "$INSTDIR\bin\tree.ico" 
!endif
SetOutPath "${CWD}"
!ifdef ABMP
  CreateShortCut "$INSTDIR\shortcuts\log2trace.lnk" "$INSTDIR\bin\log2trace.exe" 
 CreateShortCut "$INSTDIR\shortcuts\abmptr.lnk" "$INSTDIR\bin\abmptr.exe"
!endif
SetOutPath $INSTDIR
  FILE userman.html
  FILE getting_started_LEADSTO.pdf getting_started_TTL.pdf
  FILE ChangeLog
  FILE issues.html
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\LT_PL1 "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LT_PL1" "DisplayName" "Leadsto software(1) (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LT_PL1" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"
  
SectionEnd

!ifdef ABMP
Section "Abmp Trace Example"
SetOutPath $INSTDIR\abmp
FILE nego\traces\negotiation12.fm
FILE nego\traces\*.tr
CreateShortCut "$INSTDIR\shortcuts\ABMPchecker1EXAMPLEvu10.lnk" "$INSTDIR\bin\ttlchecker.exe" "-trace vu1009DAT1.tr negotiation12.fm" "$INSTDIR\bin\tree.ico" 0
CreateShortCut "$INSTDIR\shortcuts\ABMPchecker1EXAMPLEabmp03.lnk" "$INSTDIR\bin\ttlchecker.exe" "-trace abmp0311DAT1.tr negotiation12.fm" "$INSTDIR\bin\tree.ico" 0
CreateShortCut "$INSTDIR\shortcuts\ABMPcheckerALLINTERESTINGvu10.lnk" "$INSTDIR\bin\ttlchecker.exe" "-trace vu1010DAT8.tr -trace vu1009DAT2.tr -trace vu1009DAT1.tr -trace vu1006DAT7.tr negotiation12.fm" "$INSTDIR\bin\tree.ico" 0
 CreateShortCut "$INSTDIR\shortcuts\ABMPcheckerALLvu10.lnk" "$INSTDIR\bin\ttlchecker.exe" "-traces vu10*.tr  negotiation12.fm" "$INSTDIR\bin\tree.ico" 0
 CreateShortCut "$INSTDIR\shortcuts\ABMPcheckerALLabmp03.lnk" "$INSTDIR\bin\ttlchecker.exe" "-traces abmp03*.tr  negotiation12.fm" "$INSTDIR\bin\tree.ico" 0
  CreateShortCut "$INSTDIR\shortcuts\ABMPcheckerALLai02.lnk" "$INSTDIR\bin\ttlchecker.exe" "-traces ai02*.tr  negotiation12.fm" "$INSTDIR\bin\tree.ico" 0
SectionEnd
!endif

; optional section (can be disabled by the user)
Section "Start Menu Shortcuts"
  CreateDirectory "$SMPROGRAMS\ltpl1"
  CopyFiles "$INSTDIR\shortcuts\*.*" "$SMPROGRAMS\ltpl1\"
  WriteRegStr HKLM SOFTWARE\LT_PL1 cwd     ${CWD}
SectionEnd
Section "DeskTop Shortcuts"
  CreateDirectory "$DESKTOP\DESIRE shortcuts\"
  CopyFiles "$INSTDIR\shortcuts\*.*" "$DESKTOP\DESIRE shortcuts\"

  WriteRegStr HKLM SOFTWARE\LT_PL1 cwd     ${CWD}
SectionEnd

Section "Leadsto Open Extension"
  ; back up old value of .lt
  ReadRegStr $1 HKCR .lt ""
  StrCmp $1 "" Label1LT
    StrCmp $1 "LeadsToFile" Label1LT
    WriteRegStr HKCR .lt "backup_val" $1
  Label1LT:
  WriteRegStr HKCR .lt "" "LeadsToFile"

  ReadRegStr $0 HKCR "LeadsToFile" ""
  StrCmp $0 "" 0 skipLTAssoc
	WriteRegStr HKCR "LeadsToFile" "" "LeadsTo File"
	WriteRegStr HKCR "LeadsToFile\shell" "" "open"
	WriteRegStr HKCR "LeadsToFile\DefaultIcon" "" "$INSTDIR\bin\tree.ico"
  skipLTAssoc:
  ; OPEN
  WriteRegStr HKCR "LeadsToFile\shell\open\command" "" '"$INSTDIR\bin\leadsto.exe" -wd ${CWD} "%1"'
  WriteRegStr HKLM SOFTWARE\LT_PL1 fileExtensionlt lt
SectionEnd

Section "TTL Open Extension"
  ; back up old value of .lt
  ReadRegStr $1 HKCR .fm ""
  StrCmp $1 "" Label1FM
    StrCmp $1 "TTLFile" Label1FM
    WriteRegStr HKCR .fm "backup_val" $1
  Label1FM:
  WriteRegStr HKCR .fm "" "TTLFile"

  ReadRegStr $0 HKCR "TTLFile" ""
  StrCmp $0 "" 0 skipFMAssoc
	WriteRegStr HKCR "TTLFile" "" "TTL File"
	WriteRegStr HKCR "TTLFile\shell" "" "open"
	WriteRegStr HKCR "TTLFile\DefaultIcon" "" "$INSTDIR\bin\ttl.ico"
  skipFMAssoc:
  ; OPEN
  WriteRegStr HKCR "TTLFile\shell\open\command" "" '"$INSTDIR\bin\ttlchecker.exe" -wd ${CWD} "%1"'
  WriteRegStr HKLM SOFTWARE\LT_PL1 fileExtensionfm fm
SectionEnd

;--------------------------------

; Uninstaller

UninstallText "This will uninstall leadsto software(1). Hit next to continue."

; Uninstall section

Section "Uninstall"

  ReadRegStr $1   HKLM Software\LT_PL1 fileExtensionlt
  StrCmp "$1" "" LTNoExt
  ReadRegStr $1 HKCR .lt ""
  StrCmp $1 "LeadsToFile" 0 LTNoOwn ; only do this if we own it
  ReadRegStr $1 HKCR .lt "backup_val"
      StrCmp $1 "" 0 LTRestoreBackup
	DeleteRegKey HKCR .lt
      Goto LTNoOwn
  LTRestoreBackup:	
  WriteRegStr HKCR .lt "" $1
  DeleteRegValue HKCR .lt "backup_val"
  LTNoOwn:
  LTNoExt:


  ReadRegStr $1   HKLM Software\LT_PL1 fileExtensionfm
  StrCmp "$1" "" FMNoExt
  ReadRegStr $1 HKCR .fm ""
  StrCmp $1 "TTLFile" 0 FMNoOwn ; only do this if we own it
  ReadRegStr $1 HKCR .fm "backup_val"
  StrCmp $1 "" 0 FMRestoreBackup
  DeleteRegKey HKCR .fm
  Goto FMNoOwn
  FMRestoreBackup:	
  WriteRegStr HKCR .fm "" $1
  DeleteRegValue HKCR .fm "backup_val"
  FMNoOwn:
  FMNoExt:


  ; remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LT_PL1"
  DeleteRegKey HKLM SOFTWARE\LT_PL1
  ; remove files and uninstaller
  Delete $INSTDIR\bin\ltbare.exe
  Delete $INSTDIR\bin\log2trace.exe
  Delete $INSTDIR\bin\leadsto.exe
  Delete $INSTDIR\userman.html
  Delete $INSTDIR\shortcuts\*.*
  ; remove shortcuts, if any
  Delete "$SMPROGRAMS\ltpl1\*.*"
  Delete "$DESKTOP\leadsto.lnk"
  Delete "$DESKTOP\lteditor.lnk"
  Delete "$DESKTOP\ttlchecker.lnk"
  Delete "$DESKTOP\trace2log.lnk"
  ; remove directories used
  RMDir /r "$INSTDIR\shortcuts"
  RMDir /r "$SMPROGRAMS\ltpl1"
  RMDir /r "$INSTDIR"

SectionEnd


Function .onInit

  ;Extract InstallOptions files
  ;$PLUGINSDIR will automatically be removed when the installer closes
  
  InitPluginsDir
  File /oname=$PLUGINSDIR\options.ini "options.ini"

FunctionEnd


Function SetCustom
# Basic system info
  Call UserInfo


# Working Directory
  ReadRegStr ${CWD} HKLM SOFTWARE\LT_PL1 cwd
  StrCmp ${CWD} "" 0 HasCWD
    StrCpy ${CWD} ${DEFCWD}
  HasCWD:
  WriteINIStr $PLUGINSDIR\options.ini "Field 4" "State" ${CWD}
# Start the dialog
  Push ${TEMP1}
  InstallOptions::dialog "$PLUGINSDIR\options.ini"
  Pop ${TEMP1}
  Pop ${TEMP1}

# Get the results
#  ReadINIStr ${GRP} $PLUGINSDIR\options.ini "Field 6" "State"
  ReadINIStr ${CWD} $PLUGINSDIR\options.ini "Field 4" "State"
FunctionEnd


Function UserInfo
  ClearErrors
  UserInfo::GetName
  IfErrors Win9x
  Pop $0
  UserInfo::GetAccountType
  Pop $1

  StrCmp $1 "Admin" 0 +4
#    SetShellVarContext all
#    StrCpy ${SHCTX} "all"
    Goto done
  StrCmp $1 "Power" 0 +3
#    StrCpy ${SHCTX} "all"
    Goto done
  StrCmp $1 "User" 0 +3
#    StrCpy ${SHCTX} "current"
    Goto done
  StrCmp $1 "Guest" 0 +3
#    StrCpy ${SHCTX} "current"
    Goto done
#  StrCpy ${SHCTX} "current"		# Unkown accounttype
    Goto done

  Win9x:
    StrCpy ${DEFCWD} $TEMP
#    StrCpy ${SHCTX}  "current"
    Goto end

  done:
#    StrCmp ${SHCTX} "all" 0 +2
#      SetShellVarContext all
    StrCpy ${DEFCWD} $TEMP

  end:
FunctionEnd

Function .onInstSuccess
  MessageBox MB_YESNO "Installation complete. View usermanual?" IDNO NoReadme
  ExecShell "open" "$INSTDIR\userman.html"
  NoReadme:
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "Installation failed.$\r$\n\
		    If you cannot resolve the issue or it is a bug in the$\r$\n\
		    installer, please contact lourens@cs.vu.nl"
FunctionEnd
