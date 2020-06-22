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
   LOCAL oWnd, oSize
   LOCAL oVlay
   LOCAL oHlay

   WITH OBJECT oWnd := hqlMainWindow( /*name*/ )
      :hqlCaption( "HQL resources tester" ) // ==>:setWindowTitle( "tester" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget( /*cname*/ ) )

      WITH OBJECT :centralWidget()

         WITH OBJECT oVlay := hqlVboxLayout( /*name*/ )
            :hqlSetLayoutOf( oWnd:centralWidget() )

            // labels to show icons
            WITH OBJECT oHlay := hqlHboxLayout( /*name*/ )
               :hqlAddMeToLayout( oVlay )
               :addStretch()
               WITH OBJECT hqlLabel( "logosico" )
                  :hqlAddMeToLayout( oHlay )
               END WITH
               :addItem( QSpacerItem( 20, 10, QSizePolicy_Expanding ) )
               WITH OBJECT hqlLabel( "buttonsico" )
                  :hqlAddMeToLayout( oHlay )
               END WITH
               :addItem( QSpacerItem( 20, 10, QSizePolicy_Expanding ) )
               WITH OBJECT hqlLabel( "flagsico" )
                  :hqlAddMeToLayout( oHlay )
               END WITH
               :addStretch()
            END WITH

            WITH OBJECT oHlay := hqlHboxLayout( /*name*/ )
               :hqlAddMeToLayout( oVlay )
               :addStretch()
               WITH OBJECT hqlListWidget( "logoslst" )
                  :hqlOnCurrentRowChanged( { |nInt| udfShowLogos(nInt,ownd) } )
                  :hqlAddMeToLayout( oHlay )
               END WITH
               :addItem( QSpacerItem( 20, 10, QSizePolicy_Expanding ) )
               WITH OBJECT hqlListWidget( "buttonslst" )
                  :hqlOnCurrentRowChanged( { |nInt| udfShowButtons(nInt,ownd) } )
                  :hqlAddMeToLayout( oHlay )
               END WITH
               :addItem( QSpacerItem( 20, 10, QSizePolicy_Expanding ) )
               WITH OBJECT hqlListWidget( "flagslst" )
                  :hqlOnCurrentRowChanged( { |nInt| udfShowFlags(nInt,ownd) } )
                  :hqlAddMeToLayout( oHlay )
               END WITH
               :addStretch()
            END WITH

         END WITH

      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   udfLoadLogos( oWnd:logoslst() )
   udfLoadButtons( oWnd:buttonslst() )
   udfLoadFlags( oWnd:flagslst() )

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE udfShowLogos( nInt, ownd )

   LOCAL oIcon
   LOCAL oPixmap

   oIcon := QIcon( ownd:logoslst:item(nInt):text() )

   oPixmap := oIcon:pixmap( QSize( 96, 96 ), QIcon_Normal, QIcon_On )

   oWnd:logosico:setPixmap( oPixmap )

RETURN

STATIC PROCEDURE udfShowButtons( nInt, ownd )

   LOCAL oIcon
   LOCAL oPixmap

   oIcon := QIcon( ownd:buttonslst:item(nInt):text() )

   oPixmap := oIcon:pixmap( QSize( 72,72 ), QIcon_Normal, QIcon_On )

   oWnd:buttonsico:setPixmap( oPixmap )

RETURN

STATIC PROCEDURE udfShowFlags( nInt, ownd )

   LOCAL oIcon
   LOCAL oPixmap

   oIcon := QIcon( ownd:flagslst:item(nInt):text() )

   oPixmap := oIcon:pixmap( QSize( 48, 48 ), QIcon_Normal, QIcon_On )

   oWnd:flagsico:setPixmap( oPixmap )

RETURN

STATIC PROCEDURE udfLoadLogos( oList )

   oList:hqlAddRow( ":/hqlres/HQL96" )
   oList:hqlAddRow( ":/hqlres/dummy" )

RETURN

STATIC PROCEDURE udfLoadButtons( oList )

   oList:hqlAddRow( ":/hqlres/accept" )
   oList:hqlAddRow( ":/hqlres/calendar" )
   oList:hqlAddRow( ":/hqlres/clear" )
   oList:hqlAddRow( ":/hqlres/critical" )
   oList:hqlAddRow( ":/hqlres/danger" )
   oList:hqlAddRow( ":/hqlres/database" )
   oList:hqlAddRow( ":/hqlres/document" )
   oList:hqlAddRow( ":/hqlres/exit" )
   oList:hqlAddRow( ":/hqlres/eyeclose" )
   oList:hqlAddRow( ":/hqlres/eyeopen" )
   oList:hqlAddRow( ":/hqlres/exit" )
   oList:hqlAddRow( ":/hqlres/filter" )
   oList:hqlAddRow( ":/hqlres/folder" )
   oList:hqlAddRow( ":/hqlres/folderopen" )
   oList:hqlAddRow( ":/hqlres/godown" )
   oList:hqlAddRow( ":/hqlres/goleft" )
   oList:hqlAddRow( ":/hqlres/goright" )
   oList:hqlAddRow( ":/hqlres/goup" )
   oList:hqlAddRow( ":/hqlres/help" )
   oList:hqlAddRow( ":/hqlres/home" )
   oList:hqlAddRow( ":/hqlres/information" )
   oList:hqlAddRow( ":/hqlres/login" )
   oList:hqlAddRow( ":/hqlres/logout" )
   oList:hqlAddRow( ":/hqlres/pleject" )
   oList:hqlAddRow( ":/hqlres/plffw" )
   oList:hqlAddRow( ":/hqlres/plfirst" )
   oList:hqlAddRow( ":/hqlres/pllast" )
   oList:hqlAddRow( ":/hqlres/plpause" )
   oList:hqlAddRow( ":/hqlres/plplay" )
   oList:hqlAddRow( ":/hqlres/plrec" )
   oList:hqlAddRow( ":/hqlres/plrewind" )
   oList:hqlAddRow( ":/hqlres/plstop" )
   oList:hqlAddRow( ":/hqlres/print" )
   oList:hqlAddRow( ":/hqlres/question" )
   oList:hqlAddRow( ":/hqlres/quit" )
   oList:hqlAddRow( ":/hqlres/recadd" )
   oList:hqlAddRow( ":/hqlres/recdel" )
   oList:hqlAddRow( ":/hqlres/recedit" )
   oList:hqlAddRow( ":/hqlres/reclist" )
   oList:hqlAddRow( ":/hqlres/recsave" )
   oList:hqlAddRow( ":/hqlres/recselect" )
   oList:hqlAddRow( ":/hqlres/refresh" )
   oList:hqlAddRow( ":/hqlres/remove" )
   oList:hqlAddRow( ":/hqlres/save" )
   oList:hqlAddRow( ":/hqlres/search" )
   oList:hqlAddRow( ":/hqlres/sortasc" )
   oList:hqlAddRow( ":/hqlres/sortdesc" )
   oList:hqlAddRow( ":/hqlres/txtbold" )
   oList:hqlAddRow( ":/hqlres/txtcenter" )
   oList:hqlAddRow( ":/hqlres/txtcolor" )
   oList:hqlAddRow( ":/hqlres/txtfamily" )
   oList:hqlAddRow( ":/hqlres/txtdecind" )
   oList:hqlAddRow( ":/hqlres/txtincind" )
   oList:hqlAddRow( ":/hqlres/txtitalic" )
   oList:hqlAddRow( ":/hqlres/txtjust" )
   oList:hqlAddRow( ":/hqlres/txtlalign" )
   oList:hqlAddRow( ":/hqlres/txtralign" )
   oList:hqlAddRow( ":/hqlres/txtsize" )
   oList:hqlAddRow( ":/hqlres/txtstrout" )
   oList:hqlAddRow( ":/hqlres/txtunderl" )
   oList:hqlAddRow( ":/hqlres/tools" )
   oList:hqlAddRow( ":/hqlres/trash" )
   oList:hqlAddRow( ":/hqlres/undo" )
   oList:hqlAddRow( ":/hqlres/users" )
   oList:hqlAddRow( ":/hqlres/warning" )
   oList:hqlAddRow( ":/hqlres/world" )
   oList:hqlAddRow( ":/hqlres/zoom" )
   oList:hqlAddRow( ":/hqlres/zoomin" )
   oList:hqlAddRow( ":/hqlres/zoomout" )

RETURN

STATIC PROCEDURE udfLoadFlags( oList )

   oList:hqlAddRow( ":/hqlres/cf_olimpic" )
   oList:hqlAddRow( ":/hqlres/cf_at" )
   oList:hqlAddRow( ":/hqlres/cf_be" )
   oList:hqlAddRow( ":/hqlres/cf_ch" )
   oList:hqlAddRow( ":/hqlres/cf_cz" )
   oList:hqlAddRow( ":/hqlres/cf_de" )
   oList:hqlAddRow( ":/hqlres/cf_dk" )
   oList:hqlAddRow( ":/hqlres/cf_es" )
   oList:hqlAddRow( ":/hqlres/cf_eu" )
   oList:hqlAddRow( ":/hqlres/cf_fr" )
   oList:hqlAddRow( ":/hqlres/cf_gb" )
   oList:hqlAddRow( ":/hqlres/cf_gr" )
   oList:hqlAddRow( ":/hqlres/cf_ie" )
   oList:hqlAddRow( ":/hqlres/cf_is" )
   oList:hqlAddRow( ":/hqlres/cf_it" )
   oList:hqlAddRow( ":/hqlres/cf_li" )
   oList:hqlAddRow( ":/hqlres/cf_lu" )
   oList:hqlAddRow( ":/hqlres/cf_nl" )
   oList:hqlAddRow( ":/hqlres/cf_no" )
   oList:hqlAddRow( ":/hqlres/cf_pl" )
   oList:hqlAddRow( ":/hqlres/cf_pt" )
   oList:hqlAddRow( ":/hqlres/cf_se" )
   oList:hqlAddRow( ":/hqlres/cf_sk" )
   oList:hqlAddRow( ":/hqlres/cf_us" )

RETURN
