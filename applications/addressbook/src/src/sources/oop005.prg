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
#include "addrbook.ch"

/*!

 \brief main form

*/
CREATE CLASS oop005 INHERIT basic_form

   EXPORTED:
   METHOD init
   METHOD activate                        // override
   METHOD prepareUi                       // override
   METHOD refreshUi                       // override

   PROTECTED:
   DATA oAddrbook                         INIT NIL
   METHOD __createWin                     // override
   METHOD __onClose                       // override
   METHOD __loadForm
   METHOD __recAdd
   METHOD __recEdit
   METHOD __recSelect

END CLASS

/*!

 \brief initialize

*/
METHOD init( ... ) CLASS oop005

   ::basic_form:setParent( ... )

RETURN Self

/*!

 \brief

*/
METHOD activate() CLASS oop005

   IF ::oAddrbook:isOpen()
      ::oWindow:hqlActivate()
   ENDIF

RETURN Self

/*!

 \brief

*/
METHOD prepareUi() CLASS oop005

   ::oAddrbook := addrbookDm( appcnf:get( "dbfdir" ) )

   IF ::oAddrbook:open()
      ::__createWin()
   ENDIF

RETURN Self

/*!

 \brief

*/
METHOD refreshUi() CLASS oop005
   ::oWindow:grid:setRowCount( 0 )
   ::__loadForm()
RETURN Self

/*!

 \brief create window

*/
METHOD __createWin() CLASS oop005

   WITH OBJECT ::oWindow := hqlChildWindow( /*name*/, ::parentWindow() )
      :setWindowTitle("Counters")
      :resize( 600, 450 )
      :setWindowIcon( QIcon( ":/pgmico" ) )
      :setCentralWidget( hqlTableWidget( "grid" ) )
      :hqlOnActivate( { || ::__loadForm() } )
      :hqlOnClose( { || ::__onClose() } )

      WITH OBJECT hqlToolBar( /*name*/ )
         :setIconSize( QSize( 48, 48 ) )
         // to set buttons fixedsize :hqlButtonSize( QSize( .... ) or nValue or nWidth, nHeight )
         :setContextMenuPolicy( Qt_PreventContextMenu ) // to prevent rightClick to close

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { || ::oWindow:hqlRelease() } )
            :setIcon( QIcon( appcnf:get( "Exit_Icon" ) ) )
            :setToolTip( hqlTran( "appgeneral", appcnf:get( "Exit_Tooltip" ) ) )
            :hqlTabStop( .F. )
         END WITH

         :addSeparator()

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { || ::__recSelect() } )
            :setIcon( QIcon( appcnf:get( "RecSelect_Icon" ) ) )
            :setToolTip( hqlTran( "appgeneral", appcnf:get( "RecSelect_Tooltip" ) ) )
            :hqlTabStop( .F. )
         END WITH

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { || ::__recEdit() } )
            :setIcon( QIcon( appcnf:get( "RecEdit_Icon" ) ) )
            :setToolTip( hqlTran( "appgeneral", appcnf:get( "RecEdit_Tooltip" ) ) )
            :hqlTabStop( .F. )
         END WITH

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { || ::__recAdd() } )
            :setIcon( QIcon( appcnf:get( "RecAdd_Icon" ) ) )
            :setToolTip( hqlTran( "appgeneral", appcnf:get( "RecAdd_Tooltip" ) ) )
            :hqlTabStop( .F. )
         END WITH

      END WITH

      // to customize tablewidget
      WITH OBJECT :centralWidget()
         :setAlternatingRowColors( .T. )                          // alternated row color
         :setEditTriggers( QAbstractItemView_NoEditTriggers )     // not editable
         :setSelectionBehavior( QAbstractItemView_SelectRows )    // row selecting
         :setSelectionMode( QAbstractItemView_SingleSelection )   // single selection
         :horizontalHeader:setStretchLastSection( .T. )
         :setStyleSheet( appcnf:get( "disabled", "" ) )

         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( hqlTran( "appgeneral", "Code" ) )
            END WITH
            WITH OBJECT :cell()
               :setTextAlignment( Qt_AlignCenter )
            END WITH
         END WITH

         WITH OBJECT :hqlAddColumn()
            :setWidth( 200 )
            WITH OBJECT :header()
               :setText( hqlTran( "appgeneral", "Surname" ) )
            END WITH
            WITH OBJECT :cell()
               :setTextAlignment( hb_BitOr( Qt_AlignLeft, Qt_AlignVCenter ) )
            END WITH
         END WITH

         WITH OBJECT :hqlAddColumn()
            :setWidth( 200 )
            WITH OBJECT :header()
               :setText( hqlTran( "appgeneral", "Name" ) )
            END WITH
            WITH OBJECT :cell()
               :setTextAlignment( hb_BitOr( Qt_AlignLeft, Qt_AlignVCenter ) )
            END WITH
         END WITH

         WITH OBJECT :hqlAddColumn()
            WITH OBJECT :header()
               :setText( hqlTran( "appgeneral", "Born on" ) )
            END WITH
            WITH OBJECT :cell()
               :setTextAlignment( Qt_AlignCenter )
            END WITH
         END WITH

      END WITH

   END WITH

RETURN NIL

/*!

 \brief close window

*/
METHOD __onClose() CLASS oop005
   ::oAddrbook:close()
RETURN NIL

/*!

 \brief record selected

*/
METHOD __loadForm() CLASS oop005
   LOCAL oRow
   LOCAL oDate

   (::oAddrbook:wa)->(ORDSETFOCUS("bycode"))
   (::oAddrbook:wa)->(DBGOTOP())

   DO WHILE ! (::oAddrbook:wa)->(EOF())

      oRow := ::oWindow:grid:hqlAddRow()
      oRow[ 1 ]:setText( (::oAddrbook:wa)->(FIELDGET(1)) )
      oRow[ 2 ]:setText( (::oAddrbook:wa)->(FIELDGET(3)) )
      oRow[ 3 ]:setText( (::oAddrbook:wa)->(FIELDGET(2)) )
      oDate := hqlDate():fromHbString( (::oAddrbook:wa)->(FIELDGET(4)), /*format*/ )
      oRow[ 4 ]:setText( oDate:toLocaleString() )

      (::oAddrbook:wa)->(DBSKIP())
   ENDDO

RETURN NIL

/*!

 \brief record selected

*/
METHOD __recAdd() CLASS oop005

   LOCAL oProgram

   oProgram := oop006():new( Self )
   oProgram:params:setAction( FORMACTION_INSERT )
   oProgram:params:setCode( "" )
   oProgram:prepareUi()
   oProgram:activate()

RETURN NIL

/*!

 \brief record selected

*/
METHOD __recEdit() CLASS oop005

   LOCAL oRow
   LOCAL oProgram

   IF ::oWindow:grid:hqlCurrentRow() == -1
      hql_MsgWarn( hqlTran( "appgeneral", "Select a row" ) )
      RETURN .F.
   ENDIF

   oRow := ::oWindow:grid:hqlGetRow( ::oWindow:grid:hqlCurrentRow() )

   oProgram := oop006():new( Self )
   oProgram:params:setAction( FORMACTION_EDIT )
   oProgram:params:setCode( oRow[1]:text() )
   oProgram:prepareUi()
   oProgram:activate()

RETURN NIL

/*!

 \brief record selected

*/
METHOD __recSelect() CLASS oop005

RETURN NIL
