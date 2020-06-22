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

/*!

 \brief define hql_abs0000 class; basically FOR ANY object is QObject derived

*/
CLASS hql_abs0000 INHERIT hql_hqlobj

   EXPORTED:
   METHOD hqlCleaner
   METHOD hqlDestroyer
   METHOD hqlHasParent                    INLINE ( ::__hqlHasParent() )
   METHOD hqlHasParentWidget              INLINE ( ::__hqlHasParentWidget() )
   METHOD hqlSetObjectName
   METHOD hqlTr

   PROTECTED:
   VAR bHqlObjectNameChanged              INIT NIL
   METHOD __hqlAssignObjectName
   METHOD __hqlCleaner
   METHOD __hqlConnect
   METHOD __hqlDisconnect
   METHOD __hqlHasParent
   METHOD __hqlHasParentWidget
   METHOD __hqlIsDuplicateName
   METHOD __hqlNestedWithGetParent
   METHOD __hqlNestedWithSetParent
   METHOD __hqlSignalEverybody
   SIGNAL __hql_QObjectNameChanged

   HIDDEN:

ENDCLASS

/*!

 \brief Clean object
 \param(IN)
 \return self

*/
METHOD hqlCleaner() CLASS hql_abs0000
   ::__hqlCleaner()
RETURN self

/*
 \brief Object destroyer
 \param(IN)
 \return NIL

*/
METHOD hqlDestroyer() CLASS hql_abs0000
   ::__hqlSignalEverybody( self )
   ::setParent( 0 )  //hql_SetNoParent( self )
   hql_Delete( self )   // delete bind object
RETURN NIL

/*
 \brief Set object name. WARNING; harbour returns upper case (findObject) so we need to convert upeer the name
 \param(IN) string
 \return self

*/
METHOD hqlSetObjectName( arg1 ) CLASS hql_abs0000
   IF ( hb_IsString(arg1) )
      ::setObjectName( UPPER( ALLTRIM( arg1 ) ) )
   ENDIF
RETURN self

/*!

 \brief inline translation with className as context
 \param[in] ...
 \return string

*/
METHOD hqlTr( ... ) CLASS hql_abs0000
// this works fine (ie) RETURN Hql:Tran( "QLabel", ... )
// this works fine (ie) RETURN Hql:Tran( "hb_QLabel", ... )
// this works fine (ie) RETURN Hql:Tran( "hql_Label", ... )
// Self:className() returns UPPERCASE value... so, we must use (ie) <name>HQL_LABEL</name> into .ts
RETURN HqlFw:translate( Self:className(), ... )

// ==================== PROTECTED section ====================

/*
 \brief [PROTECTED] Assign object name
 \param(IN)
 \return NIL

*/
METHOD __hqlAssignObjectName( cString ) CLASS hql_abs0000
   cString := UPPER( ALLTRIM( hb_DefaultValue(cString, "") ) )

   IF ( LEN(cString) == 0 .AND. HqlFw:autoNameEnabled() )
      cString := HqlFw:getAutoName()
   ENDIF

   IF ( LEN(cString) > 0 )
      IF ( ::__hqlHasParent() .AND. ::__hqlIsDuplicateName( cString, ::parent() ) )
         hqlThrow( hqlErrorNew( 7003, PROCNAME(), { cString } ) )
      ENDIF
      ::setObjectName( cString )
   ENDIF

RETURN NIL

/*

 \brief [PROTECTED] Object cleaner
 \param(IN)
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_abs0000
   // do something
RETURN NIL

/*!

 \brief [PROTECTD] Connect signal
 \param(IN)
 \return bool

*/
METHOD __hqlConnect() CLASS hql_abs0000
   ::connect( "objectNameChanged(QString)", { |cString| ::__hql_QObjectNameChanged(cString) } )
RETURN .T.

/*!

 \brief [PROTECTED] disconnect everything. WARNING: it check HbQtObjectHandler var[s]
 \param(IN)
 \return NIL

*/
METHOD __hqlDisconnect() CLASS hql_abs0000
   IF ( __objHasMsg( Self, "disconnect" ) .AND. ( !EMPTY(::__hEvents) .OR. !EMPTY(::__Slots) ) )
      __objSendMsg( Self, "disconnect" )
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTD] Returns true if object has a parent otherwise false
 \param(IN)
 \return bool

*/
METHOD __hqlHasParent() CLASS hql_abs0000
   IF ( __objHasMsg(self, "parent") )
      RETURN ( hb_IsObject(::parent()) )
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTD] Returns true if object has a parentWidget otherwise false
 \param(IN)
 \return bool

*/
METHOD __hqlHasParentWidget() CLASS hql_abs0000
   IF ( __objHasMsg(self, "parentWidget") )
      RETURN ( hb_IsObject(::parentWidget()) )
   ENDIF
RETURN .F.

/*!

 \brief [PROTECTD] Returns true if name already exists in parent
 \param(IN)
 \return bool

*/
METHOD __hqlIsDuplicateName( cString, oParent ) CLASS hql_abs0000
   LOCAL lDuplicated := .F.
   LOCAL nAt

   cString := UPPER( ALLTRIM( hb_DefaultValue(cString, "") ) )

   IF ( LEN(cstring) > 0 .AND. hb_IsObject(oParent) .AND. oParent:isDerivedFrom("QObject") )
      FOR nAt := 0 TO (oParent:children:size()-1)
         IF ( oParent:children:at(nAt):isDerivedFrom("QObject") .AND. UPPER( ALLTRIM( oParent:children:at(nAt):objectName() ) ) == cString )
            lDuplicated := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

RETURN lDuplicated

/*!

 \brief [PROTECTD] Try to find parent using WITH nesting
 \param(IN)
 \return object | NIL

*/
METHOD __hqlNestedWithGetParent() CLASS hql_abs0000
   LOCAL oTemp

   // if object is *LAYOUT derived, better to return NIL; will be Qt parental system that will reparent object[s].
   IF ( ::isDerivedFrom("QLayout") )
      RETURN NIL
   ENDIF

   IF ( hb_IsObject(oTemp := HQL_QWITH()) )
      IF ( oTemp:isDerivedFrom("QLayout") )
         RETURN oTemp:parentWidget()
      ENDIF
      // to be ensure is a Qt object :-)
      IF ( oTemp:isDerivedFrom("QObject") )
         RETURN oTemp
      ENDIF
   ENDIF

RETURN NIL

/*!

 \brief [PROTECTD] Try to set parent using WITH nesting
 \param(IN)
 \return NIL

*/
METHOD __hqlNestedWithSetParent() CLASS hql_abs0000
   LOCAL oParent
   IF ( __objHasMsg(self, "setParent") .AND. hb_IsObject( oParent := ::__hqlNestedWithGetParent() ) )
      ::setParent( oParent )
   ENDIF
RETURN NIL

/*!

 \brief [PROTECTED] clean every childs
 \param(IN) object
 \return NIL

*/
METHOD __hqlSignalEverybody( oInputObject, nIndent ) CLASS hql_abs0000
   LOCAL oList, nAt

   nIndent := hb_DefaultValue(nIndent, 0)

/*hql_Trace( SPACE(nIndent) + "__hqlSignalEverybody START: " + oInputObject:className() )*/

   IF ( oInputObject:isDerivedFrom( "QObject" ) )
/*hql_Trace( SPACE(nIndent) + "__hqlSignalEverybody childrens:" + hb_NtoS(oInputObject:children():count()) )*/
      IF ( oInputObject:children():count() > 0 )
         oList := oInputObject:children()
         FOR nAt := (oList:count()-1) TO 0 STEP -1
            IF ( oList:at(nAt):isDerivedFrom("QWidget") .AND. oList:at(nAt):isWindow() )
/*hql_Trace( SPACE(nIndent) + "__hqlSignalEverybody is a window, call close" )*/
               oList:at(nAt):close()
            ENDIF
/*hql_Trace( SPACE(nIndent) + "__hqlSignalEverybody for children" )*/
            ::__hqlSignalEverybody( oList:at(nAt), nIndent+3 )
         NEXT
      ENDIF
/*hql_Trace( SPACE(nIndent) + "__hqlSignalEverybody disconnect" )*/
      oInputObject:disconnect()
   ENDIF

   IF ( oInputObject:isDerivedFrom( "hql_abs0000" ) )
/*hql_Trace( SPACE(nIndent) + "__hqlSignalEverybody hqlCleaner" )*/
      oInputObject:hqlCleaner()
   ENDIF

/*hql_Trace( SPACE(nIndent) + "__hqlSignalEverybody END: " + oInputObject:className() )*/
HB_SYMBOL_UNUSED(nIndent)
RETURN NIL

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
 \param(IN) ...
 \return bool

*/
METHOD __hql_QObjectNameChanged( cString ) CLASS hql_abs0000
   IF hb_IsEvalItem( ::bHqlObjectNameChanged )
      EVAL( ::bHqlObjectNameChanged, cString, Self )  // always last Self argument
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
