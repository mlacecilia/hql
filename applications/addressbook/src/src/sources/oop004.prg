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
CREATE CLASS oop004 INHERIT basic_form

   EXPORTED:
   METHOD init
   METHOD activate                        // override
   METHOD params                          INLINE ::oParams
   METHOD prepareUi                       // override

   PROTECTED:
   DATA oCounters                         INIT NIL
   DATA oParams                           INIT NIL
   METHOD __createWin                     // override
   METHOD __onClose                       // override
   METHOD __onActivate                    // override
   METHOD __loadForm
   METHOD __recSave
   METHOD __tryToManage

END CLASS

/*!

 \brief initialize

*/
METHOD init( ... ) CLASS oop004

   ::basic_form:setParent( ... )
   ::oParams := p004_cargo():new()

RETURN Self

/*!

 \brief

*/
METHOD activate() CLASS oop004

   IF ::oCounters:isOpen()
      ::oWindow:hqlActivate()
   ENDIF

RETURN Self

/*!

 \brief

*/
METHOD prepareUi() CLASS oop004

   ::oCounters := countersDm( appcnf:get( "dbfdir" ) )
   ::oCounters:open()

   IF ::oCounters:isOpen()
      ::__createWin()
      ::oWindow:cntcodeid:hqlValue( ::params:code() )
   ENDIF

RETURN Self

/*!

 \brief create window

*/
METHOD __createWin() CLASS oop004
   LOCAL oMlayout, oHlay

   WITH OBJECT ::oWindow := hqlChildWindow( /*name*/, ::parentWindow() )
      :setWindowTitle("Counters")
      :resize( 600, 450 )
      :setWindowIcon( QIcon( ":/pgmico" ) )
      :setCentralWidget( hqlWidget(/*name*/, ::oWindow) )
      :centralWidget():setLayout( hqlVBoxLayout() )
      :hqlOnActivate( { || ::__onActivate() } )
      :hqlOnClose( { || ::__onClose() } )

      WITH OBJECT hqlToolBar( /*name*/ )
         :setIconSize( QSize( 48, 48 ) )
         :setContextMenuPolicy( Qt_PreventContextMenu ) // to prevent rightClick to close

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { || ::oWindow:hqlRelease() } )
            :setIcon( QIcon( appcnf:get( "Exit_Icon" ) ) )
            :setToolTip( hqlTran( "appgeneral", appcnf:get( "Exit_Tooltip" ) ) )
            :hqlTabStop( .F. )
         END WITH

         :addSeparator()

         WITH OBJECT :hqlAddToolButton( /*name OR toolButton*/ )
            :hqlOnClicked( { || ::__recSave() } )
            :setIcon( QIcon( appcnf:get( "RecSave_Icon" ) ) )
            :setToolTip( hqlTran( "appgeneral", appcnf:get( "RecSave_Tooltip" ) ) )
            :hqlTabStop( .F. )
         END WITH

      END WITH

      WITH OBJECT :centralWidget()
         oMlayout := :layout()

         oHlay := hqlHBoxLayout()
         oHlay:setContentsMargins( 0, 0, 0, 0 )
         WITH OBJECT hqlLabel()
            :hqlAddMeToLayout( oHlay )
            :setMinimumWidth( 110 )
            :setAlignment( Qt_AlignRight+Qt_AlignVCenter )
            :setText( hqlTran( "appgeneral", "Code" ) )
         END WITH
         WITH OBJECT hqlInputText( "cntcodeid" )
            :setStyleSheet( appcnf:get( "disabled", "" ) )
            :hqlAddMeToLayout( oHlay )
            :setMaximumWidth( 120 )
            :hqlInputMask( ">Nnnnnnnnnn" )
            :hqlOnTabPressed( { || ::__tryToManage() } )
         END WITH
         oHlay:addStretch()   /* pushLeft */
         oMlayout:addLayout( oHlay )

         oHlay := hqlHBoxLayout()
         oHlay:setContentsMargins( 0, 0, 0, 0 )
         WITH OBJECT hqlLabel()
            :hqlAddMeToLayout( oHlay )
            :setMinimumWidth( 110 )
            :setAlignment( Qt_AlignRight+Qt_AlignVCenter )
            :setText( hqlTran( "appgeneral", "Description" ) )
         END WITH
         WITH OBJECT hqlInputText( "cntdescri" )
            :setStyleSheet( appcnf:get( "disabled", "" ) )
            :hqlAddMeToLayout( oHlay )
            :setMaxLength( 50 )
            :setEnabled( .F. )
         END WITH
         oHlay:addStretch()   /* pushLeft */
         oMlayout:addLayout( oHlay )

         oHlay := hqlHBoxLayout()
         oHlay:setContentsMargins( 0, 0, 0, 0 )
         WITH OBJECT hqlLabel()
            :hqlAddMeToLayout( oHlay )
            :setMinimumWidth( 110 )
            :setAlignment( Qt_AlignRight+Qt_AlignVCenter )
            :setText( hqlTran( "appgeneral", "Value" ) )
         END WITH
         WITH OBJECT hqlInputNumeric( "cntamount" )
            :setStyleSheet( appcnf:get( "disabled", "" ) )
            :hqlAddMeToLayout( oHlay )
            :hqlInputMask( "99,999,999" )
            :setMaximumWidth( 120 )
            :setEnabled( .F. )
         END WITH
         oHlay:addStretch()   /* pushLeft */
         oMlayout:addLayout( oHlay )
         oMlayout:addStretch()   /* pushUp */

      END WITH

   END WITH

RETURN NIL

/*!

 \brief close window

*/
METHOD __onActivate() CLASS oop004

   IF ::params:action() == FORMACTION_EDIT
      ::__tryToManage()
//   ELSE
//      ::__loadForm()
   ENDIF

RETURN NIL

/*!

 \brief close window

*/
METHOD __onClose() CLASS oop004
   ::oCounters:close()
RETURN NIL

/*!

 \brief save current record

*/
METHOD __loadForm() CLASS oop004

   IF (::oCounters:wa)->(FOUND())
      ::oWindow:cntcodeid:hqlValue( ALLTRIM( (::oCounters:wa)->(FIELDGET(1)) ) )
      ::oWindow:cntdescri:hqlValue( ALLTRIM( (::oCounters:wa)->(FIELDGET(2)) ) )
      ::oWindow:cntamount:hqlValue( (::oCounters:wa)->(FIELDGET(3)) )
   ENDIF

   ::oWindow:cntcodeid:setEnabled( .F. )
   ::oWindow:cntdescri:setEnabled( .T. )
   ::oWindow:cntamount:setEnabled( .T. )

   // while editing mode I need to set focus; while inserting I press TAB: not required
   IF !( ::params:action() == FORMACTION_INSERT )
      ::oWindow:cntdescri:hqlSetFocus()
   ENDIF

RETURN NIL

/*!

 \brief save current record

*/
METHOD __recSave() CLASS oop004

   IF ! (::oCounters:wa)->(FOUND())
      (::oCounters:wa)->(DBAPPEND())
      IF NETERR()
         hql_MsgStop( hqlTran( "apperror", "Record can't be append" ), NIL, NIL, QIcon( ":/pgmico" ) )
         RETURN NIL
      ENDIF
   ENDIF

   (::oCounters:wa)->(FIELDPUT(1, ::oWindow:cntcodeid:hqlValue()))
   (::oCounters:wa)->(FIELDPUT(2, ::oWindow:cntdescri:hqlValue()))
   (::oCounters:wa)->(FIELDPUT(3, ::oWindow:cntamount:hqlValue()))
   (::oCounters:wa)->(DBCOMMIT())

   IF ::hasParent() .AND. __objHasMsg( ::parent(), "refreshUi" )
      ::parent:refreshUi()
   ENDIF

   ::oWindow:hqlRelease()

RETURN NIL

/*!

 \brief try to read, lock and load form

*/
METHOD __tryToManage() CLASS oop004

   LOCAL cKey

   cKey := SUBSTR( ::oWindow:cntcodeid:hqlValue() + SPACE( 10 ), 1, 10 )
   (::oCounters:wa)->(ORDSETFOCUS("bycode"))
   IF (::oCounters:wa)->(DBSEEK(cKey,.F.))
      IF (::oCounters:wa)->(DBRLOCK())
         ::__loadForm()
         RETURN NIL
      ENDIF
   ENDIF

   IF ! (::oCounters:wa)->(FOUND()) .AND. ::params:action() == FORMACTION_INSERT
      ::__loadForm()
      RETURN NIL
   ENDIF

   hql_MsgStop( hqlTran( "apperror", "Record not found or not locked" ), NIL, NIL, QIcon( ":/pgmico" ) )

   ::oWindow:hqlRelease()

RETURN NIL

//////////////////////////////////////// \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CREATE CLASS p004_cargo

   EXPORTED:
   METHOD init
   METHOD action                          INLINE ::nAction
   METHOD code                            INLINE ::cCode
   METHOD setAction
   METHOD setCode

   PROTECTED:
   DATA nAction                           INIT FORMACTION_NONE
   DATA cCode                             INIT ""

END CLASS

METHOD init() CLASS p004_cargo
RETURN Self

METHOD setAction( ... ) CLASS p004_cargo
   IF hb_IsNumeric( hb_Pvalue(1) ) .AND. hb_Pvalue(1) >= FORMACTION_MIN .AND. hb_Pvalue(1) <= FORMACTION_MAX
      ::nAction := hb_Pvalue(1)
   ENDIF
RETURN Self

METHOD setCode( ... ) CLASS p004_cargo
   IF hb_IsString( hb_Pvalue(1) )
      ::cCode := hb_Pvalue(1)
   ENDIF
RETURN Self
