/****************************************************************************
** $HQL_BEGIN_LICENSE$
**
** Copyright (C) 2020 by Luigi Ferraris
**
** This file is part of HQL project
**
** HQL is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** HQL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with HQL.  If not, see <http://www.gnu.org/licenses/>.
**
** $HQL_END_LICENSE$
****************************************************************************/
#include "addrbook.ch"

/*!

 \brief starting procedure

*/
INIT PROCEDURE ThisInit()

   hb_setTermCP( hb_cdpTerm() )           //where <cTermCP> is OS encoding and <cHostCP> is HVM encoding. When <cHostCP> is not given then _SET_CODEPAGE is used
   SET( _SET_OSCODEPAGE, hb_CdpOS() )
   SET( _SET_DBCODEPAGE, "ITWIN" )        // I choose Italian

   SET( _SET_EPOCH, 2000 )
   SET CENTURY ON
   SET( _SET_EXCLUSIVE, .F. )
   SET( _SET_DELETED, .F. )

RETURN

/*!

 \brief starting procedure

*/
EXIT PROCEDURE ThisExit()

   DBCOMMITALL()
   DBCLOSEALL()

RETURN

/*

   standard main function

*/
PROCEDURE Main( ... )
   LOCAL oFont

   hqlErrorSys()

   hqlSetStyle( "Fusion" )
   // QApplication:setStyleSheet break style: is a documented problem
   //hqlLoadQss( ":/stylesheet/addrbook.qss" ) // embedded stylesheet file

   UDFmakeConfig()

   IF ( !UDFmakeDirs() )
      RETURN
   ENDIF

   oFont := hqlQApplication:font()
   oFont:setPointSize( 12 )
   hqlQApplication:setFont( oFont )

   hqlStart()

   runMain()

RETURN

STATIC PROCEDURE runMain()
   LOCAL oProgram

   oProgram := oop001():new()

   oProgram:prepareUi()

   oProgram:activate()

RETURN

STATIC PROCEDURE UDFmakeConfig()
   LOCAL cPath
   LOCAL cFile
   LOCAL cExt

   hb_FnameSplit( hb_ProgName(), @cPath, @cFile, @cExt )

   // set up configuration
   appcnf:set( "workdir", cPath )
   appcnf:set( "datadir", hb_PathNormalize( cPath + hb_ps() + "data" ) )
   appcnf:set( "dbfdir", hb_PathNormalize( appcnf:get( "datadir" ) + hb_ps() + "dbfs" ) )
   // I will use external file translations to keep lite .exe size
   appcnf:set( "trandir", hb_PathNormalize( appcnf:get( "datadir" ) + hb_ps() + "translations" ) )

   // load buttons hql images
   appcnf:set( "Clear_Caption", "Clear" )
   appcnf:set( "Clear_Tooltip", "Clear" )
   appcnf:set( "Clear_Icon", ":/hqlres/clear" )

   appcnf:set( "Exit_Caption", "Exit" )
   appcnf:set( "Exit_Tooltip", "Exit" )
   appcnf:set( "Exit_Icon", ":/hqlres/exit" )

   appcnf:set( "Quit_Caption", "Quit" )
   appcnf:set( "Quit_Tooltip", "Quit" )
   appcnf:set( "Quit_Icon", ":/hqlres/quit" )

   appcnf:set( "RecAdd_Caption", "RecAdd" )
   appcnf:set( "RecAdd_Tooltip", "Add record" )
   appcnf:set( "RecAdd_Icon", ":/hqlres/recadd" )

   appcnf:set( "RecDel_Caption", "RecDel" )
   appcnf:set( "RecDel_Tooltip", "Delete record" )
   appcnf:set( "RecDel_Icon", ":/hqlres/recdel" )

   appcnf:set( "RecEdit_Caption", "RecEdit" )
   appcnf:set( "RecEdit_Tooltip", "Edit record" )
   appcnf:set( "RecEdit_Icon", ":/hqlres/recedit" )

   appcnf:set( "RecList_Caption", "RecList" )
   appcnf:set( "RecList_Tooltip", "Record list" )
   appcnf:set( "RecList_Icon", ":/hqlres/reclist" )

   appcnf:set( "RecSave_Caption", "RecSave" )
   appcnf:set( "RecSave_Tooltip", "Save record" )
   appcnf:set( "RecSave_Icon", ":/hqlres/recsave" )

   appcnf:set( "RecSelect_Caption", "RecSelect" )
   appcnf:set( "RecSelect_Tooltip", "Select record" )
   appcnf:set( "RecSelect_Icon", ":/hqlres/recselect" )

   appcnf:set( "Search_Caption", "Search" )
   appcnf:set( "Search_Tooltip", "Start search" )
   appcnf:set( "Search_Icon", ":/hqlres/search" )

   appcnf:set( "Tick_Caption", "Apply" )
   appcnf:set( "Tick_Tooltip", "Apply" )
   appcnf:set( "Tick_Icon", ":/hqlres/tick" )

   appcnf:set( "disabled", ':disabled {background-color: #D1D1FF; color: #0000D1;}' )

   // default Qr translations: we need only give file prefix "qt_". We assume files are located in a path defined by qt.conf file
   hqlTranslations:addItem( "qt_" )
   // set hql translations prefix and related external directory
   hqlTranslations:addItem( "addrbook_", appcnf:get( "trandir" ) )

RETURN

STATIC FUNCTION UDFmakeDirs()

   IF ( !hb_VfDirExists( appcnf:get( "datadir" ) ) )
      IF ( hb_VfDirMake( appcnf:get( "datadir" ) ) != 0 )
         hql_MsgStop( hqlTran( "apperror", "Directories structure doesn't exists" ), "Error", appcnf:get( "datadir" ), /*cInfoText*/ )
         RETURN .F.
      ENDIF
   ENDIF

   IF ( !hb_VfDirExists( appcnf:get( "dbfdir" ) ) )
      IF ( hb_VfDirMake( appcnf:get( "dbfdir" ) ) != 0 )
         hql_MsgStop( hqlTran( "apperror", "Can't create folder" ), "Error", appcnf:get( "dbfdir" ), /*cInfoText*/ )
         RETURN .F.
      ENDIF
   ENDIF

RETURN .T.
