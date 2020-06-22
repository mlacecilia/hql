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
#include "hqlinclude.ch"
#include "cardgames.ch"

/*!

 \brief starting procedure

*/
INIT PROCEDURE ThisInit()

   hb_CdpSelect( hb_CdpOS() )    /* to align HVM to os codePage */
   hb_setTermCP( hb_cdpTerm() )  /* where <cTermCP> is OS encoding and <cHostCP> is HVM encoding. When <cHostCP> is not given then _SET_CODEPAGE is used */
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

   standard main procedure

*/
PROCEDURE Main()
   LOCAL oFont

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlSetStyle( "Fusion" ) /* Windows, WindowsXP, WindowsVista */

   // default Qr translations: we need only give file prefix "qt_". We assume files are located in a path defined by qt.conf file
   hqlTranslations:addItem( "qt_" )

   // registers current program resources
   hqlRegisterResData( hbqtres_defres() )

   hqlQApplication:setWindowIcon( QIcon( ":pgmico" ) )
   hqlQApplication:setOrganizationName( "HQL" )
   hqlQApplication:setOrganizationDomain( "l3w.it" )
   hqlQApplication:setApplicationName( "cardgames" )

   hqlSconfig():set( "homedir", hb_Cwd() )
   hqlSconfig():set( "cardsdir", hb_PathNormalize(hqlSconfig():get( "homedir", "." ) + hb_Ps() + "cards" ) )
   hqlSconfig():set( "carpet", "#0A6C03" )
   hqlSconfig():set( "checked", "#FF6600" )

   hqlStart()

   /*
      fixed font oFont := Qfont("Monospace")
      fixed font oFont:setStyleHint(  QFont_TypeWriter )
      to increase whoel application font size
      oFont := QFont()
      oFont:setFamily( oFont:defaultFamily() )
   */
   oFont := hqlQapplication:font()
   oFont:setPointSize(10)
   hqlQApplication:setFont(oFont)

   UDFshowMainForm()

RETURN

/*!

 \brief show main form

*/
STATIC PROCEDURE UDFshowMainForm()
   LOCAL oMain
   oMain := mainForm()
   oMain:activate()
RETURN
