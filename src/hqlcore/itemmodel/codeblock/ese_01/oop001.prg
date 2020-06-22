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
#include "hbqtgui.ch"
#include "hbqtcore.ch"

#include "hbclass.ch"
#include "hbtrace.ch"

/*!

 \brief

*/
EXIT PROCEDURE __thisExit()
   IF ! ( __hbqt_itemsInGlobalList() == 0 )
      __hbqt_dump_itemsInGlobalList()
   ENDIF
RETURN

/*

   standard main function

   http://www.thedazzlersinc.com/source/2012/06/04/qt-qtableview-example-short-and-quick/
   https://stackoverflow.com/questions/8946127/refreshing-column-header-names-in-qt-model
   http://doc.qt.io/qt-5/sql-presenting.html
   http://doc.qt.io/qt-4.8/modelview.html#2-4-setting-up-headers-for-columns-and-rows

   http://doc.qt.io/qt-5/qstandarditem.html


   itemdelegate sembra che HbQt definisca virtual i metodi
      http://www.bogotobogo.com/Qt/Qt5_QTableView_QItemDelegate_ModelView_MVC.php

QSortFilterProxyModel* proxyModel = new QSortFilterProxyModel(tview);
proxyModel->setSourceModel(model);
tview->setModel(proxyModel);


*/
PROCEDURE Main( ... )

   hbqt_errorsys()

   ShowWindow()

RETURN

PROCEDURE ShowWindow()

   LOCAL oWnd

   WITH OBJECT oWnd := hqlMainWindow()
      :setWindowTitle("HQL test resources")
      :resize( 800, 600 )
      :setCentralWidget( hqlWidget() )

      WITH OBJECT :centralWidget()

         WITH OBJECT hqlPushButton()
            :setGeometry( 10, 10, 150, 30 )
            :setText( "addColumn" )
            :hqlOnClicked( { || udfAddColumn( oWnd:tview() ) } )
         END WITH

         WITH OBJECT hqlPushButton()
            :setGeometry( 170, 10, 150, 30 )
            :setText( "addRow" )
            :hqlOnClicked( { || udfAddRow( oWnd:tview() ) } )
         END WITH

         WITH OBJECT hqlPushButton()
            :setGeometry( 330, 10, 150, 30 )
            :setText( "clear" )
            :hqlOnClicked( { || oWnd:tview:model:clear() } )
         END WITH

         WITH OBJECT hqlPushButton()
            :setGeometry( 490, 10, 150, 30 )
            :setText( "info" )
            :hqlOnClicked( { || udfInfo( oWnd:tview() ) } )
         END WITH

//         WITH OBJECT hqlTableView()
         WITH OBJECT myBrowse()
            :setGeometry( 10, 90, 700, 400 )
            :setObjectName( "TVIEW" )
         END WITH

      END WITH

   END WITH

   oWnd:hqlActivate()

RETURN

STATIC PROCEDURE udfAddRow( oTview )

   LOCAL oIndex
   LOCAL nRow
   LOCAL nCol
   LOCAL cAutomticText

/*
   // this test is required because ::index( row, col ) always returns a valid QModelindex
   // probably we need to create a callback
   IF oTview:model:columnCount() > 0 .AND. oTview:model:rowCount() >= 0

      oTview:model:addRow()   // create required space for a new row
      nRow := oTview:model:rowCount() - 1

      FOR nCol := 0 TO oTview:model:columnCount() - 1

         cAutomticText := "cell_" + hb_NtoS( nRow ) + "_" + hb_NtoS( nCol )

         oIndex := oTview:model:index( nRow, nCol, QModelIndex() )

         oTview:model:setData( oIndex, QVariant( cAutomticText ), Qt_DisplayRole )

      NEXT nCol

   ENDIF

*/
   // I have overriden ::index() method into my_model but oIndex:isValid() test is required
   oTview:model:addRow()   // create required space for a new row
   nRow := oTview:model:rowCount()-1
   FOR nCol := 0 TO oTview:model:columnCount() - 1
      oIndex := oTview:model:index( nRow, nCol, QModelIndex() )
      IF oIndex:isValid()
         cAutomticText := "cell_" + hb_NtoS( nRow ) + "_" + hb_NtoS( nCol )
         oTview:model:setData( oIndex, QVariant( cAutomticText ), Qt_DisplayRole )
      ENDIF
   NEXT

RETURN

STATIC PROCEDURE udfAddColumn( oTview )

   LOCAL cAutomticText

   oTview:model:addColumn()
   cAutomticText := "col_" + hb_NtoS( oTview:model:columnCount() )
   oTview:model:setHeaderData( oTview:model:columnCount()-1, Qt_Horizontal, QVariant( cAutomticText ), Qt_DisplayRole )
   oTview:model:setHeaderData( oTview:model:columnCount()-1, Qt_Horizontal, QVariant( Qt_AlignCenter ), Qt_TextAlignmentRole )

RETURN

STATIC PROCEDURE udfInfo( oTview )

   LOCAL nRow
   LOCAL nColumn
   LOCAL oIndex

   hb_trace( HB_TR_ALWAYS, " horizontalHeader sections =" + hb_NtoS( oTview:horizontalHeader:count() ) + ;
                           " verticalHeader sections =" + hb_NtoS( oTview:verticalHeader:count() ) + ;
                           " model rowCount =" + hb_NtoS( oTview:model:rowCount() ) + ;
                           " model columnCount =" + hb_NtoS( oTview:model:columnCount() ) )


   hb_trace( HB_TR_ALWAYS, "headerData =" + oTview:model:headerData( Qt_DisplayRole, Qt_Horizontal, 0 ):toString() )

   FOR nRow := 0 TO oTview:model:rowCount() - 1

      FOR nColumn := 0 TO oTview:model:columnCount() - 1

         oIndex := oTview:model:index( nRow, nColumn, QModelIndex() )

         hb_trace( HB_TR_ALWAYS, " row=" + hb_NtoS( nRow ) + ;
                                 " col=" + hb_NtoS( nColumn ) + ;
                                 " text=" + oTview:model:data( oIndex, Qt_DisplayRole ):toString() )
      NEXT nColumn

   NEXT nRow

RETURN
