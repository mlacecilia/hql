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
#include "hqlinclude.ch"

/*!

 \brief [PUBLIC function] delete widget
 \param(IN) object
 \return NIL

*/
FUNCTION hql_DeleteWidget( oObject )
   IF ( hql_IsHqlAbsDerived( oObject ) )
      oObject:hqlDestroyer()
   ELSEIF( hql_IsQobjectDerived( oObject ) )
      oObject:disconnect()
      oObject:setParent( 0 )  //hql_SetNoParent( oObject )
      hql_Delete( oObject )      // delete bind object
   ENDIF
RETURN NIL

/*!

 \brief [PUBLIC function] returns true if IS object and IS hql_hqlobj derived
 \param(IN) object
 \return bool

*/
FUNCTION hql_IsHqlDerived( oObject )
RETURN ( hb_IsObject(oObject) .AND. oObject:isDerivedFrom("hql_hqlobj") )

/*!

 \brief [PUBLIC function] returns true if IS object and IS QObject derived
 \param(IN) object
 \return bool

*/
FUNCTION hql_IsQobjectDerived( oObject )
RETURN ( hb_IsObject(oObject) .AND. oObject:isDerivedFrom("QObject") )

/*!

 \brief [PUBLIC function] returns true if IS object and IS hql_abs0000 derived (IOW the most low level QObject in Hql)
 \param(IN) object
 \return bool

*/
FUNCTION hql_IsHqlAbsDerived( oObject )
RETURN ( hb_IsObject(oObject) .AND. oObject:isDerivedFrom("hql_abs0000") )

/*!

 \brief [PUBLIC function] returns true if object has a parent
 \param(IN) object
 \return boolean

*/
FUNCTION hql_HasParent( oObject )
RETURN ( hql_IsQobjectDerived( oObject ) .AND. hb_IsObject( oObject:parent() ) )

/*!

 \brief [PUBLIC function] returns true if object has a parentWidget
 \param(IN) object
 \return boolean

*/
FUNCTION hql_HasParentWidget( oObject )
RETURN ( hql_IsQobjectDerived( oObject ) .AND. __objHasMsg( oObject, "parentWidget" ) .AND. hb_IsObject( oObject:parentWidget() ) )


/*!

 \brief [PUBLIC_FUNCTION] returns true if IS object and IS derived from className
   note: Harbour always convert into UPPER case the className string for find it, so it's not required to use UPPER
 \param(IN) object, string
 \return bool

*/
FUNCTION hql_IsDerived( oObject, cClassName )
   cClassName := hb_DefaultValue(cClassName, "")
RETURN ( hb_IsObject(oObject) .AND. LEN(cClassName) > 0 .AND. oObject:isDerivedFrom(cClassName) )

/*!

 \brief [PUBLIC_FUNCTION] returns true if IS object and has className
   note: Harbour always returns className in UPPER case, so it's required to use UPPER
 \param(IN) object, string
 \return bool

*/
FUNCTION hql_IsClass( oObject, cClassName )
   cClassName := UPPER(hb_DefaultValue(cClassName, ""))
RETURN ( hb_IsObject(oObject) .AND. LEN(cClassName) > 0 .AND. UPPER(oObject:className()) == cClassName )

/*!

 \brief [PUBLIC_FUNCTION] returns true if string1 in in string2; string2 can be a list Harbour supported, e.g. "abc|def|ghi..."
 \param(IN) string, string
   string must be "abc|def|...."
 \return bool

*/
FUNCTION hql_IsInList( cString, cStringList )
   cString := hb_DefaultValue(cString, "")
   cStringList := hb_DefaultValue(cStringList, "")
   IF ( EMPTY(cString) .OR. EMPTY(cStringList) )
      // wrong arguments
      RETURN .F.
   ENDIF
RETURN ( "|" + ( cString ) + "|" $ "|" + ( cStringList ) + "|" )

/*!

 \brief [PUBLIC_PROCEDURE] to explore QObject childrens
 \param(IN) QObject, bool, numeric
 \return

*/
PROCEDURE hql_exploreChildren( oObject, lChildrens, nTab )
   LOCAL nAt
   LOCAL oList
   lChildrens := hb_DefaultValue(lChildrens, .F. )
   nTab := hb_DefaultValue(nTab, 0 )
   IF ( hql_IsQobjectDerived( oObject ) )
      oList := oObject:children()
      hql_Trace( SPACE(nTab) + " className: " + oObject:className() + " objectName: " + oObject:objectName() + " childrens: " + hb_NtoS(oList:size()) )
      IF ( lChildrens )
         FOR nAt := 0 TO (oList:size() - 1)
            hql_exploreChildren( oList:at(nAt), lChildrens, nTab+3 )
         NEXT nAt
      ENDIF
   ENDIF
RETURN

/*!

 \brief [PUBLIC_PROCEDURE] Explores top level widgets
 \param(IN)
 \return

*/
PROCEDURE hql_exploreTopLevelWidgets( lChildrens )
   LOCAL nAt
   LOCAL oList := hqlQApplication:topLevelWidgets()
   lChildrens := hb_DefaultValue(lChildrens, .F. )
   FOR nAt := 0 TO (oList:count() - 1)
      hql_Trace( "topLevel: " + hb_NtoS(nAt) + " " + ;
                 "className: " + oList:at(nAt):className() + " " + ;
                 "objectName: " + oList:at(nAt):objectName() + " " + ;
                 "childrens: " + hb_NtoS(oList:at(nAt):children():size()) )
      IF ( lChildrens )
         hql_exploreChildren( oList:at(nAt), lChildrens, 3 )
      ENDIF
   NEXT
RETURN

/*!

 \brief [PUBLIC_FUNCTION] returns QColor converting HARBOUR_array
 \param(IN) HARBOUR_array
 \return QColor

*/
FUNCTION hql_ArrayToColor( aColor )
   LOCAL nR := 0
   LOCAL nG := 0
   LOCAL nB := 0
   LOCAL nA := 255

   IF ( hb_IsArray(aColor) .AND. LEN(aColor) >= 3 )
      nR := hb_DefaultValue(aColor[1], 0)
      nR := IIF( nR >= 0 .AND. nR <= 255, nR, 0 )
      nG := hb_DefaultValue(aColor[2], 0)
      nG := IIF( nG >= 0 .AND. nG <= 255, nG, 0 )
      nB := hb_DefaultValue(aColor[3], 0)
      nB := IIF( nB >= 0 .AND. nB <= 255, nB, 0 )
      IF ( LEN(aColor) >= 4 )
         nA := hb_DefaultValue(aColor[4], 0)
         nA := IIF( nA >= 0 .AND. nA <= 255, nA, 255 )
      ENDIF
   ENDIF

RETURN QColor( nR, nG, nB, nA )

/*!

 \brief [PUBLIC_FUNCTION] returns string replacing numbered arguments
 \param(IN) string message, argument[s]
 \return string

*/
FUNCTION hql_Sprintf( cMessage, ... )

   LOCAL nArg
   LOCAL cSearch
   LOCAL cReplace

   hb_Default( @cMessage, "" )

   FOR nArg := 2 TO PCOUNT()

      cSearch := "%" + hb_NtoC( nArg - 1 )

      SWITCH ( VALTYPE( hb_Pvalue( nArg ) ) )
      CASE "C"
      CASE "M"
         cReplace := hb_Pvalue( nArg )
         EXIT
      CASE "N"
         cReplace := hb_NtoC( hb_Pvalue( nArg ) )
         EXIT
      CASE "D"
         cReplace := hb_DtoC( hb_Pvalue( nArg ), "YYYYMMDD" )
         EXIT
      CASE "L"
         cReplace := IIF( hb_Pvalue( nArg ), "true", "false" )
         EXIT
      CASE "T"
         cReplace := hb_Ttos( hb_Pvalue( nArg ) )
         EXIT
      OTHERWISE
         cReplace := cSearch
         EXIT
      END CASE

      cMessage := STRTRAN( cMessage, cSearch, cReplace )

   NEXT nArg

RETURN cMessage

/*!

 \brief [PUBLIC_FUNCTION] returns true if given argument is a SPACE(1)
 \param(IN) ...
 \return bool

*/
FUNCTION hql_IsSpace( arg1 )
RETURN ( hb_IsString(arg1) .AND. arg1 == SPACE(1) )

/*!

 \brief [PUBLIC_FUNCTION] returns true if given argument is a valid QIcon
 \param(IN) ...
 \return bool

*/
FUNCTION hql_IsValidIcon( ... )
   LOCAL oIcon := QIcon( ... )
RETURN ( IIF (oIcon:isNull(), .F., .T.) )

/*!

 \brief [PUBLIC_FUNCTION] it calls Harbour function
 \param(IN) bool
 \return bool

*/
FUNCTION hql_SetCentury( ... )
RETURN __SetCentury( ... )

/*!

 \brief [PUBLIC_FUNCTION] transform number following hqlInputNumeric logic
 \param(IN) numeric [,inputMask]
 \return string

*/
FUNCTION hql_Transform( nNumeric, cMask, lAlwaysSigned )
   LOCAL cDecSep := hb_Uchar(HqlFw:QtLocale():decimalPoint():unicode())
   LOCAL cChar, cDummy
   LOCAL cString
   LOCAL nI

   nNumeric := hb_DefaultValue(nNumeric, 0)
   cMask := hb_DefaultValue(cMask, "")
   lAlwaysSigned := hb_DefaultValue(lAlwaysSigned, .F.)  // extension to always show sign

   IF ( !EMPTY( cMask ) )
      // following hql_inputNumeric inputmask
      cDummy := ""
      FOR EACH cChar IN cMask
         IF ( cChar $ "9.," )
            cDummy += cChar
         ENDIF
      NEXT
      cMask := cDummy
   ENDIF

   IF ( EMPTY( cMask ) )
      cString := hb_NtoS(nNumeric)
      IF ( HqlFw:QtLocale():decimalPoint():unicode() == 44 )   // ","
         cString := STRTRAN(cString, ".", cDecSep)
      ENDIF
   ELSE
      IF ( HqlFw:QtLocale():decimalPoint():unicode() == 44 )   // ","
         cMask := "@E " + cMask
      ENDIF
      cString := TRANSFORM(nNumeric, cMask)
   ENDIF
   // extension
   IF ( lAlwaysSigned .AND. nNumeric > 0 .AND. (nI := RAT(SPACE(1),cString)) > 0 )
      cString := STUFF(cString, nI, 1, "+")
   ENDIF

RETURN cString

/*!

 \brief [PUBLIC_FUNCTION] unTransform string into numeric value following hqlInputNumeric logic
 \param(IN) string
 \return numeric

*/
FUNCTION hql_Utransform( cString )
   LOCAL cDecSep := hb_Uchar(HqlFw:QtLocale():decimalPoint():unicode())
   LOCAL cGrpSep := hb_Uchar(HqlFw:QtLocale():groupSeparator():unicode())
   cString := hb_DefaultValue(cString, "")
   cString := STRTRAN(cString, cGrpSep, "")  // always remove group separator (thousands)
   IF ( HqlFw:QtLocale():decimalPoint():unicode() == 44 )   // decimal separator is ","
      cString := STRTRAN(cString, cDecSep, ".") // replace decimal separator with USA separator
   ENDIF
RETURN VAL(ALLTRIM(cString))
