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
#include "hbclass.ch"
#include "hqlhbqt.ch"

FUNCTION dragWidget( ... )
RETURN my_dragWidget():new( ... )

/*!

 \brief my_dragWidget class definition

   http://doc.qt.io/qt-5/dnd.html Qt foundamentals
   http://doc.qt.io/qt-5/examples-draganddrop.html
   http://doc.qt.io/qt-5/qtwidgets-draganddrop-dropsite-example.html examples
   https://wiki.qt.io/Drag_and_Drop_of_files

   http://www.qtcentre.org/threads/37753-Drag-and-Drop-items-in-QListWidget

*/
CREATE CLASS my_dragWidget INHERIT hql_listWidget

   FRIEND CLASS my_dragWidget

   EXPORTED:
   METHOD init

   PROTECTED:
   DATA oDropHintItem                     INIT NIL
   METHOD __hqlConnect                    // override
   METHOD __hql_QDragEnter
   METHOD __hql_QDragMove
   METHOD __hql_QDragLeave
   METHOD __hql_QDrop

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS my_dragWidget

   QMimeData()  // trick to solve problem HB_QMIMEDATA function not found

   ::hql_listWidget:init( ... )

   ::setAcceptDrops( .T. )
   ::setDragEnabled( .T. )
   ::setDropIndicatorShown( .T. )
   ::setDragDropMode( QAbstractItemView_InternalMove )

   ::setSelectionMode(  QAbstractItemView_SingleSelection )
   ::setAlternatingRowColors( .T. )

   ::oDropHintItem := QListWidgetItem( "Drop Files here...", Self )

RETURN Self

// ==================== PROTECTED section ====================

METHOD __hqlConnect() CLASS my_dragWidget

   ::hql_listWidget:__hqlConnect()

   ::connect( QEvent_DragEnter, { |oevent| ::__hql_QDragEnter( oevent ) } )   //QDragEnterEvent
   ::connect( QEvent_DragLeave, { |oevent| ::__hql_QDragLeave( oevent ) } )   //QDragLeaveEvent
   ::connect( QEvent_DragMove,  { |oevent| ::__hql_QDragMove( oevent ) } )    //QDragMoveEvent
   ::connect( QEvent_Drop,      { |oevent| ::__hql_QDrop( oevent ) } )        //QDropEvent

RETURN .T.

// ==================== SLOTS/EVENTS section ====================

METHOD __hql_QDragEnter( oEvent ) CLASS my_dragWidget

   LOCAL oQmimeData

   oQmimeData := oEvent:mimeData()

   IF ( oQmimeData:hasUrls() )
      oEvent:acceptProposedAction()
      RETURN .T.
   ENDIF

RETURN .F.

METHOD __hql_QDragLeave( oEvent ) CLASS my_dragWidget
   oEvent:accept()
RETURN .T.

METHOD __hql_QDragMove( oEvent ) CLASS my_dragWidget

   LOCAL oQmimeData

   oQmimeData := oEvent:mimeData()

   IF ( oQmimeData:hasUrls() )
      oEvent:acceptProposedAction()
      RETURN .T.
   ENDIF

RETURN .F.

METHOD __hql_QDrop( oEvent ) CLASS my_dragWidget

   LOCAL oQlist
   LOCAL nFile
   LOCAL oQurl
   LOCAL oQmimeData

   oQmimeData := oEvent:mimeData()

   IF ( oQmimeData:hasUrls() )

      oQlist := oQmimeData:urls()

      IF ( !oQlist:isEmpty() )

         IF ( hb_IsObject( ::oDropHintItem ) )
            ::oDropHintItem := NIL
            ::clear()
         ENDIF

         FOR nFile := 0 TO oQlist:size()-1
            oQurl := oQlist:at( nFile )
            QListWidgetItem( oQurl:toLocalFile(), Self )
         NEXT nFile

        oEvent:acceptProposedAction()

        RETURN .T.

      ENDIF

   ENDIF

RETURN .F.
