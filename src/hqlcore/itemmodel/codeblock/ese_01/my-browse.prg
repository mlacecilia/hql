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
#include "hbtrace.ch"

#include "hbqtgui.ch"
#include "hbqtcore.ch"

FUNCTION myBrowse( ... )
RETURN my_browse():new( ... )

/*!

 \brief my_browse class definition

*/
CLASS my_browse INHERIT hql_tableview

   EXPORTED:
   METHOD init
   METHOD hqlCleaner                      // override

   PROTECTED:
   DATA oDelegate                         INIT NIL
   METHOD __editor_closeEditor
   METHOD __editor_commitData

   HIDDEN:
   METHOD __h_hqlCleaner

ENDCLASS

/*!

 \brief initialize object instance

*/
METHOD init( ... ) CLASS my_browse

   ::hql_tableview:init( ... )

   ::setModel( my_model():new( Self ) )


   ::oDelegate := QItemDelegate( Self )
   ::oDelegate:connect( "closeEditor(QWidget*,QAbstractItemDelegate::EndEditHint)", { |owdg, nflag| ::__editor_closeEditor( owdg, nflag ) } )
   ::oDelegate:connect( "commitData(QWidget*)", {|owdg| ::__editor_commitData( owdg ) } )

   // hbQt QItemDelegate is not able to perform: createEditor, setEditorData, setModelData, etc.
   // so it's seems unuseful if item has a widget (eg lineedit) with signal editingFinished..
   // as simple/good example see http://www.bogotobogo.com/Qt/Qt5_QTableView_QItemDelegate_ModelView_MVC.php

RETURN Self

/*!

 \brief callable Hql cleaner
 \param[in] none
 \return NIL

*/
METHOD hqlCleaner() CLASS my_browse
   ::__h_hqlCleaner()
RETURN NIL

// ==================== PROTECTED section ====================

METHOD __editor_closeEditor( owdg, nflag ) CLASS my_browse

   hb_trace( HB_TR_ALWAYS, "classname=" + owdg:className() + " flag=" + hb_NtoS( nflag ) )

RETURN NIL

METHOD __editor_commitData( owdg ) CLASS my_browse

   hb_trace( HB_TR_ALWAYS, "classname=" + owdg:className() )

RETURN NIL

// ==================== HIDDEN section ====================

/*!

 \brief this class cleaner
 \param[in] none
 \return NIL

*/
METHOD __h_hqlCleaner() CLASS my_browse

   IF hb_IsObject( ::model() )
      ::model:hqlCleaner()
   ENDIF

   //??? errore metodo non trovato ::oDelegate:disconnect()
   ::oDelegate := NIL

   ::hql_tableview:hqlCleaner()

RETURN NIL
