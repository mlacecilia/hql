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

/*

   standard main function

*/
PROCEDURE oop003( oParent )
   LOCAL oDlg, oMlayout
   LOCAL oHlay

   WITH OBJECT oDlg := hqlChildDialog( /*name*/, oParent )
      :setWindowTitle("HQL drag&drop tablewidget, left to right only")
      :resize( 640, 480 )
      :setWindowIcon( QIcon( ":/hqlres/HQL96" ) )
      :setLayout( hqlVBoxLayout() )
      oMlayout := :layout()

      WITH OBJECT hqlMenuBar()
         :hqlAddMeToLayout( oMlayout )
         WITH OBJECT :hqlAddMenu( /*name*/ )
            :setTitle( "&File" )
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Quit" )
               :setIcon( QIcon( ":/hqlres/quit" ) )
               :setShortcut( QKeySequence( "Alt+Q" ) )
               :hqlOnTriggered( { || oDlg:hqlRelease() } )
            END WITH
            :addSeparator()
            WITH OBJECT :hqlAddAction( /*name*/ )
               :setText( "&Info" )
               :hqlOnTriggered( { || udfInfo( oDlg ) } )
            END WITH
         END WITH
      END WITH

      oHlay := hqlHBoxLayout()
      oMlayout:addLayout( oHlay )

      WITH OBJECT hqlTableWidget( "tab01" )
         :hqlAddMeToLayout( oHlay )
         :setSelectionMode(  QAbstractItemView_SingleSelection )
         :setSelectionBehavior( QAbstractItemView_SelectRows )    // row selecting else single item
         :setAlternatingRowColors( .T. )
         /* drag & drop from left to right -START- */
         :viewport:setAcceptDrops( .F. )
         :setAcceptDrops( .F. )
         :setDragEnabled( .T. )
         :setDropIndicatorShown( .T. )
         :setDragDropMode( QAbstractItemView_DragOnly )
         :setDefaultDropAction( Qt_MoveAction )
         :setDragDropOverwriteMode( .F. )
         /* drag & drop from left to right -END- */

         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( "name" )
               :setBackground( QBrush( QColor( Qt_red ) ) )
            END WITH
         END WITH
         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( "date" )
               :setBackground( QBrush( QColor( Qt_green ) ) )
            END WITH
         END WITH
         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( "hints" )
               :setBackground( QBrush( QColor( Qt_blue ) ) )
            END WITH
         END WITH

      END WITH //tab01

      oHlay:insertSpacerItem( -1, QSpacerItem( 40, 10, QSizePolicy_Fixed, QSizePolicy_Expanding ) )

      WITH OBJECT hqlTableWidget( "tab02" )
         :hqlAddMeToLayout( oHlay )
         :setSelectionMode(  QAbstractItemView_SingleSelection )
         :setSelectionBehavior( QAbstractItemView_SelectRows )    // row selecting else single item
         :setAlternatingRowColors( .T. )
         /* drag & drop from left to right -START- */
         :viewport:setAcceptDrops( .T. )
         :setAcceptDrops( .T. )
         :setDragEnabled( .F. )
         :setDropIndicatorShown( .T. )
         :setDragDropMode( QAbstractItemView_DropOnly )
         :setDefaultDropAction( Qt_MoveAction )
         :setDragDropOverwriteMode( .F. )
         /* drag & drop from left to right -END- */

         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( "name" )
               :setBackground( QBrush( QColor( Qt_red ) ) )
            END WITH
         END WITH
         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( "date" )
               :setBackground( QBrush( QColor( Qt_green ) ) )
            END WITH
         END WITH
         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( "hints" )
               :setBackground( QBrush( QColor( Qt_blue ) ) )
            END WITH
         END WITH

      END WITH //tab02

   END WITH

   udfAddRow( oDlg:tab01() )

   oDlg:hqlActivate()

RETURN

STATIC PROCEDURE udfInfo( oDlg )

   LOCAL oItem
   LOCAL nFlags

   oItem := oDlg:tab01:item( 0, 0 )
   nFlags := oItem:flags()
   hql_Trace( "flags=" + hb_NtoS( nFlags ) )

   IF hb_bitAnd( nFlags, Qt_ItemIsDropEnabled ) == Qt_ItemIsDropEnabled
      hql_Trace( "esiste" )
      nFlags -= Qt_ItemIsDropEnabled
      hql_Trace( "flags=" + hb_NtoS( nFlags ) )
   ENDIF

RETURN

STATIC PROCEDURE udfAddRow( oTableWidget )

   LOCAL nCounter
   LOCAL oRow
   LOCAL oDate
   LOCAL cNumber

   FOR nCounter := 1 TO 30

      oRow := oTableWidget:hqlAddRow()

      oRow[ 1 ]:setText( "name_" + hb_NtoS( nCounter ) )

      oDate := hqlDate( hb_Date( 1964, 1, 31 ) + nCounter - 1 )
      oRow[ 2 ]:setText( oDate:toLocaleString() )

      cNumber := hql_Transform( (nCounter * 10000), /*cMask*/ )
      oRow[ 3 ]:setText( cNumber )

   NEXT nCounter

RETURN
