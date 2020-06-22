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

   /* ALERT HbQt error. TO bypass never used before */
   QMimeData()

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
   LOCAL oMlayout
   LOCAL oButtonsLayout

   WITH OBJECT oWnd := hqlMainWindow(/*name*/)
      :setWindowTitle( "DRAG & DROP example" )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setCentralWidget( hqlWidget(/*name*/) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      /* Drag & Drop the window will be the parent who receive drag&drop */
      :setAcceptDrops( .T. )
      :connect( QEvent_DragEnter, { |oevent| __udf_QDragEnter( oevent ) } )         //QDragEnterEvent
      :connect( QEvent_Drop,      { |oevent| __udf_QDrop( oevent, oWnd:tedit() ) } )       //QDropEvent

      WITH OBJECT hqlMenuBar(/*name*/)
         WITH OBJECT :hqlAddMenu(/*name*/)
            :hqlCaption( "&File" ) //==>:setTitle( "&File" )
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Quit" )  //==>:setText( "&Quit" )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || oWnd:hqlRelease() } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction(/*name*/)
               :hqlCaption( "&Close all windows" )
               :hqlOnTriggered( { || hqlQapplication:closeAllWindows() } )
               :setIcon( QIcon( ":/hqlres/exit" ) )
            END WITH
         END WITH
      END WITH
   END WITH

   WITH OBJECT oWnd:centralWidget()
      oMlayout := :layout()

      oButtonsLayout := hqlHboxLayout()
      oButtonsLayout:addStretch()
      WITH OBJECT hqlPushButton( /*name*/ )
         :hqlAddMeToLayout( oButtonsLayout )
         :setText( "clear" )
         :hqlOnClicked( { || oWnd:tedit:clear() } )
      END WITH
      oButtonsLayout:addStretch()
      oMlayout:addLayout( oButtonsLayout )

      WITH OBJECT hqlTextEdit( "tedit" )
         :setAcceptDrops( .F. )  // by default QtextEdit accept textual from other application and if it is a file, file name will be inserted
         :hqlAddMeToLayout( oMlayout )
      END WITH

   END WITH

   // trick to resize window at 90% of desktop
   oSize := HqlQDesktop:availableGeometry():size()
   oSize := QSize( oSize:width()*0.9, oSize:height()*0.9 )
   oWnd:resize( oSize )

   oWnd:hqlActivate()

RETURN

// ==================== SLOTS/EVENTS section ====================

STATIC FUNCTION __udf_QDragEnter( oEvent )

   LOCAL oQmimeData

   oQmimeData := oEvent:mimeData()

   IF ( oQmimeData:hasFormat( "text/uri-list" ) ) //  or IF ( oQmimeData:hasUrls() )
      oEvent:acceptProposedAction()
      RETURN .T.
   ENDIF

RETURN .F.      // .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

STATIC FUNCTION __udf_QDrop( oEvent, oTedit )

   LOCAL oQmimeData
   LOCAL oQlist
   LOCAL cFileName
   LOCAL oQfile
   LOCAL oQtextStream

   oQmimeData := oEvent:mimeData()

   IF ( oQmimeData:hasUrls() )

      oQlist := oQmimeData:urls()

      IF oQlist:size() > 0

         IF ! EMPTY( cFileName := oQlist:first:toLocalFile() ) // we take only the first file

            oQfile := QFile()
            oQfile:setFileName( cFileName )

            // here we can check if fileName can be accepted (eg check extension)

            IF oQfile:exists()

               IF oQfile:open( hb_BitOr( QIODevice_ReadOnly, QIODevice_Text ) )
                  oQtextStream := QTextStream( oQfile )
                  oTedit:setPlainText( oQtextStream:readAll() )

                  oEvent:acceptProposedAction()
                  RETURN .T.

               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ENDIF

RETURN .F.      // .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
