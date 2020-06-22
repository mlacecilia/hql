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
CREATE CLASS oop006 INHERIT basic_form

   EXPORTED:
   METHOD init
   METHOD activate                        // override
   METHOD params                          INLINE ::oParams
   METHOD prepareUi                       // override

   PROTECTED:
   DATA oAddrbook                         INIT NIL
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
METHOD init( ... ) CLASS oop006

   ::basic_form:setParent( ... )
   ::oParams := p006_cargo():new()

RETURN Self

/*!

 \brief

*/
METHOD activate() CLASS oop006

   IF ::oAddrbook:isOpen()
      ::oWindow:hqlActivate()
   ENDIF

RETURN Self

/*!

 \brief

*/
METHOD prepareUi() CLASS oop006

   ::oAddrbook := addrbookDm( appcnf:get( "dbfdir" ) )
   ::oAddrbook:open()

   IF ::oAddrbook:isOpen()
      ::__createWin()
      ::oWindow:bokcodeid:hqlValue( ::params:code() )
   ENDIF

RETURN Self

/*!

 \brief create window

*/
METHOD __createWin() CLASS oop006
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

      // to customize tablewidget
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
         WITH OBJECT hqlInputText( "bokcodeid" )
            :setStyleSheet( appcnf:get( "disabled", "" ) )
            :setMaximumWidth( 100 )
            :hqlAddMeToLayout( oHlay )
            :hqlInputMask( ">Nnnnnnnnnn" )
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
            :setText( hqlTran( "appgeneral", "Surname" ) )
         END WITH
         WITH OBJECT hqlInputText( "boklsname" )
            :setStyleSheet( appcnf:get( "disabled", "" ) )
            :hqlAddMeToLayout( oHlay )
            :setMaxLength( 50 )
         END WITH
         oHlay:addStretch()   /* pushLeft */
         oMlayout:addLayout( oHlay )

         oHlay := hqlHBoxLayout()
         oHlay:setContentsMargins( 0, 0, 0, 0 )
         WITH OBJECT hqlLabel()
            :hqlAddMeToLayout( oHlay )
            :setMinimumWidth( 110 )
            :setAlignment( Qt_AlignRight+Qt_AlignVCenter )
            :setText( hqlTran( "appgeneral", "Name" ) )
         END WITH
         WITH OBJECT hqlInputText( "bokfsname" )
            :setStyleSheet( appcnf:get( "disabled", "" ) )
            :hqlAddMeToLayout( oHlay )
            :setMaxLength( 50 )
         END WITH
         oHlay:addStretch()   /* pushLeft */
         oMlayout:addLayout( oHlay )

         oHlay := hqlHBoxLayout()
         oHlay:setContentsMargins( 0, 0, 0, 0 )
         WITH OBJECT hqlLabel()
            :hqlAddMeToLayout( oHlay )
            :setMinimumWidth( 110 )
            :setAlignment( Qt_AlignRight+Qt_AlignVCenter )
            :setText( hqlTran( "appgeneral", "Born on" ) )
         END WITH
//         WITH OBJECT hqlInputDateC( "bokbrdate" )
//            :setStyleSheet( appcnf:get( "disabled", "" ) )
//            :setMaximumWidth( 120 )
//            :hqlAddMeToLayout( oHlay )
//            :hqlInputMask( "dd/mm/yyyy" )
//         END WITH
         WITH OBJECT hqlDateEdit( "bokbrdate" )
            :setStyleSheet( appcnf:get( "disabled", "" ) )
            :hqlAddMeToLayout( oHlay )
            :setCalendarPopup( .T. )
            :hqlSetCurrent()
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
METHOD __onActivate() CLASS oop006

   IF ::params:action() == FORMACTION_EDIT
      ::__tryToManage()
   ELSE
      ::__loadForm()
   ENDIF

RETURN NIL

/*!

 \brief close window

*/
METHOD __onClose() CLASS oop006
   ::oAddrbook:close()
RETURN NIL

/*!

 \brief save current record

*/
METHOD __loadForm() CLASS oop006
//   LOCAL oDate := hqlDate()

   IF (::oAddrbook:wa)->(FOUND())
      ::oWindow:bokcodeid:hqlValue( ALLTRIM( (::oAddrbook:wa)->(FIELDGET(1)) ) )
      ::oWindow:boklsname:hqlValue( ALLTRIM( (::oAddrbook:wa)->(FIELDGET(3)) ) )
      ::oWindow:bokfsname:hqlValue( ALLTRIM( (::oAddrbook:wa)->(FIELDGET(2)) ) )
//      oDate:hqlSetDate( (::oAddrbook:wa)->(FIELDGET(4)) ) // using CL..per value ("D")
//      ::oWindow:bokbrdate:hqlValue( oDate:QtDate() ) // assign using Qt object
      ::oWindow:bokbrdate:hqlStoD( (::oAddrbook:wa)->(FIELDGET(4)) )
   ENDIF

   // while editing mode I need to set focus; while inserting I press TAB: not required
//   IF !( ::params:action() == FORMACTION_INSERT )
//      ::oWindow:boklsname:hqlSetFocus()
//   ENDIF

RETURN NIL

/*!

 \brief save current record

*/
METHOD __recSave() CLASS oop006

   LOCAL cCode

   IF ! (::oAddrbook:wa)->(FOUND())
      IF udfGetCode( @cCode )
         (::oAddrbook:wa)->(DBAPPEND())
         IF NETERR()
            hql_MsgStop( hqlTran( "apperror", "Record can't be append" ), NIL, NIL, QIcon( ":/pgmico" ) )
            RETURN NIL
         ENDIF
         ::oWindow:bokcodeid:hqlValue( cCode )
      ELSE
         hql_MsgStop( hqlTran( "apperror", "Counter unavailabel" ), NIL, NIL, QIcon( ":/pgmico" ) )
      ENDIF
   ENDIF

   (::oAddrbook:wa)->(FIELDPUT( 1, ::oWindow:bokcodeid:hqlValue()))
   (::oAddrbook:wa)->(FIELDPUT( 3, ::oWindow:boklsname:hqlValue()))
   (::oAddrbook:wa)->(FIELDPUT( 2, ::oWindow:bokfsname:hqlValue()))
//   (::oAddrbook:wa)->(FIELDPUT( 4, DTOS( ::oWindow:bokbrdate:hqlValue())))
   (::oAddrbook:wa)->(FIELDPUT( 4, ::oWindow:bokbrdate:hqlDtoS()))
   (::oAddrbook:wa)->(DBCOMMIT())

   IF ::hasParent() .AND. __objHasMsg( ::parent(), "refreshUi" )
      ::parent:refreshUi()
   ENDIF

   ::oWindow:hqlRelease()

RETURN NIL

/*!

 \brief try to read, lock and load form

*/
METHOD __tryToManage() CLASS oop006

   LOCAL cKey

   cKey := SUBSTR( ::oWindow:bokcodeid:hqlValue() + SPACE( 10 ), 1, 10 )
   (::oAddrbook:wa)->(ORDSETFOCUS("bycode"))
   IF (::oAddrbook:wa)->(DBSEEK(cKey,.F.))
      IF (::oAddrbook:wa)->(DBRLOCK())
         ::__loadForm()
         RETURN NIL
      ENDIF
   ENDIF

   IF ! (::oAddrbook:wa)->(FOUND()) .AND. ::params:action() == FORMACTION_INSERT
      ::__loadForm()
      RETURN NIL
   ENDIF

   hql_MsgStop( hqlTran( "apperror", "Record not found or not locked" ), NIL, NIL, QIcon( ":/pgmico" ) )

   ::oWindow:hqlRelease()

RETURN NIL

//////////////////////////////////////// \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CREATE CLASS p006_cargo

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

METHOD init() CLASS p006_cargo
RETURN Self

METHOD setAction( ... ) CLASS p006_cargo
   IF hb_IsNumeric( hb_Pvalue(1) ) .AND. hb_Pvalue(1) >= FORMACTION_MIN .AND. hb_Pvalue(1) <= FORMACTION_MAX
      ::nAction := hb_Pvalue(1)
   ENDIF
RETURN Self

METHOD setCode( ... ) CLASS p006_cargo
   IF hb_IsString( hb_Pvalue(1) )
      ::cCode := hb_Pvalue(1)
   ENDIF
RETURN Self

//////////////////////////////////////// \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

STATIC FUNCTION udfGetCode( /*@*/cCode )

   LOCAL oCounters
   LOCAL nValue

   hb_Default( @cCode, "" )

   oCounters := countersDm( appcnf:get( "dbfdir" ) )

   IF oCounters:open()

      (oCounters:wa)->(ORDSETFOCUS("bycode"))

      IF (oCounters:wa)->(DBSEEK("LSTCONTACT",.F.))
         IF (oCounters:wa)->(DBRLOCK())
            nValue := (oCounters:wa)->(FIELDGET(3)) + 1
            (oCounters:wa)->(FIELDPUT( 3, nValue))
            (oCounters:wa)->(DBCOMMIT())
            cCode := STRZERO( nValue, 8 )
            RETURN .T.
         ELSE
            RETURN .F.
         ENDIF
      ENDIF

      (oCounters:wa)->(DBAPPEND())
      IF NETERR()
         RETURN .F.
      ENDIF

      nValue := 1
      (oCounters:wa)->(FIELDPUT( 1, "LSTCONTACT"))
      (oCounters:wa)->(FIELDPUT( 2, "contacts counter"))
      (oCounters:wa)->(FIELDPUT( 3, nValue ))
      (oCounters:wa)->(DBCOMMIT())
      cCode := STRZERO( nValue, 8 )
      RETURN .T.

   ENDIF

RETURN .F.
