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

/*!

 \brief starting procedure

*/
INIT PROCEDURE ThisInit()

   hb_CdpSelect( hb_CdpOS() )    // to align HVM to os codePage
   hb_SetTermCP( hb_CdpTerm() )  //where <cTermCP> is OS encoding and <cHostCP> is HVM encoding. When <cHostCP> is not given then _SET_CODEPAGE is used
   SET( _SET_OSCODEPAGE, hb_CdpOS() )
   SET( _SET_DBCODEPAGE, "ITWIN" )        // I choose Italian

   SET( _SET_EPOCH, 2000 )
   SET CENTURY ON
   SET( _SET_EXCLUSIVE, .F. )
   SET( _SET_DELETED, .F. )

RETURN

/*!

 \brief ending procedure

*/
EXIT PROCEDURE ThisExit()

   DBCOMMITALL()
   DBCLOSEALL()

RETURN

/*

   standard main procedure

*/
PROCEDURE Main()

   hqlErrorSys()  /*hbqt_errorsys()*/

   hqlSetStyle( "Fusion" )

   hqlOnAboutToQuit( { || UDFOnAboutToQuit() } )

   // HOW TO REGISTER RESOURCE[S] ALWAYS before :start() Hql framework

   // EXTERNAL .rcc file
   // in this scenario, extres.rcc MUST BE INTO the program directory
   hql_Trace( hb_ValToExp( hqlRegisterResFile( hb_FnameMerge( hb_DirBase(), "extres", "rcc" ) ) ) )

   // EMBEDDED resources
   // Harbour create a function that returns rccData (more or less like rcc -name option)
   // the function name is composed like "hbqtres_" + <fileName> where <fileName> is base name of file <fileName.qrc>
   // cToBeRegistered := "hbqtres_" + "embres" + "()"
   // hql_Trace( hb_ValToExp( oHqlMain:registerResData( &cToBeRegistered ) ) )
   hql_Trace( hb_ValToExp( hqlRegisterResData( hbqtres_embres() ) ) )

   hqlStart()

   UDFshowMainWindow()

RETURN

STATIC PROCEDURE UDFOnAboutToQuit()
   hql_Trace( PADR("Quitting QApplication", 25) + hb_TtoS(hb_DateTime()) )
RETURN

/*!

 \brief show mainwindow

*/
STATIC PROCEDURE UDFshowMainWindow()
   LOCAL oWnd
   LOCAL oIcon

   WITH OBJECT oWnd := hqlMainWindow( /*name*/ )
      :hqlCaption( "HQL resources tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*cname*/ ) )
      :resize( 800, 600 )

      WITH OBJECT :centralWidget()

         // see embres.qrc file  <qresource prefix="/emb"> <file alias="image1">resources/internal.png</file> </qresource>
         oIcon := QIcon( ":/emb/image1" )
         WITH OBJECT hqlLabel( /*name*/ )
            :setGeometry( 10, 20, 128, 128 )
            :setPixmap( oIcon:pixmap( QSize( 96, 96 ), QIcon_Normal, QIcon_On ) )
         END WITH

         WITH OBJECT hqlLabel( /*name*/ )
            :setGeometry( 160, 20, 340, 128 )
            // :setWordWrap( .T. )
            :setText( "PROGRAM embedded resource[s]" )
         END WITH

         // see extres.qrc file  <qresource prefix="/ext"> <file alias="image1">resources/external.png</file> </qresource>
         oIcon := QIcon( ":/ext/image1" )
         WITH OBJECT hqlLabel( /*name*/ )
            :setGeometry( 10, 180, 128, 128 )
            :setPixmap( oIcon:pixmap( QSize( 96, 96 ), QIcon_Normal, QIcon_On ) )
         END WITH

         WITH OBJECT hqlLabel( /*name*/ )
            :setGeometry( 160, 180, 340, 128 )
            // :setWordWrap( .T. )
            :setText( "PROGRAM external resource[s]. To create .rcc from .qrc (eg)" + hb_Eol() + ;
                      "<Qt_bin_dir>"+hb_Ps()+"rcc -binary -o <outputFileName>.rcc <inputFileName>.qrc" )
         END WITH

         // Hql embedded resources hqlresources.qrc <qresource prefix="/hqlres"> <file alias="tools">tools.png</file>
         oIcon := QIcon( ":/hqlres/tools" )
         WITH OBJECT hqlLabel( /*name*/ )
            :setGeometry( 10, 340, 128, 128 )
            :setPixmap( oIcon:pixmap( QSize( 96, 96 ), QIcon_Normal, QIcon_On ) )
         END WITH

         WITH OBJECT hqlLabel( /*name*/ )
            :setGeometry( 160, 340, 340, 128 )
            // :setWordWrap( .T. )
            :setText( "Hql embedded resource[s]" )
         END WITH

      END WITH

   END WITH

   oWnd:hqlActivate()

RETURN
