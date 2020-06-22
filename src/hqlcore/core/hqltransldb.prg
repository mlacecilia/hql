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
#include "hqlinclude.ch"

/*!

 \brief hql_translDb class definition

*/
CLASS hql_translDb

   EXPORTED:
   METHOD init
   METHOD addItem
   METHOD at
   METHOD deleteItem
   METHOD install
   METHOD remove
   METHOD size                            INLINE ( LEN(::aItems) )

   PROTECTED:
   VAR aItems                             INIT {}
   METHOD __indexOf
   METHOD __installTranslators
   METHOD __removeTranslators

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD init() CLASS hql_translDb
RETURN self

/*!

 \brief Add a new item
 \param(IN) string, string
 \return self

*/
METHOD addItem( cPrefix, cPath ) CLASS hql_translDb
   cPrefix := hb_DefaultValue(cPrefix, "")
   cPath := hb_DefaultValue(cPath, "")
   IF ( LEN(ALLTRIM(cPrefix)) > 0 .AND. ::__indexOf(cPrefix) == 0 )
      AADD( ::aItems, hql_translItem():new(cPrefix, cPath) )
   ENDIF
RETURN self

/*!

 \brief Returns item at index
 \param(IN) numeric
 \return self

*/
METHOD at( arg1 ) CLASS hql_translDb
   IF ( hb_IsNumeric(arg1) .AND. arg1 > 0 .AND. arg1 <= LEN(::aItems) )
      RETURN ::aItems[arg1]
   ENDIF
RETURN NIL

/*!

 \brief Remove item
 \param(IN) numeric | string
 \return self

*/
METHOD deleteItem( arg1 ) CLASS hql_translDb
   LOCAL nAt := 0
   IF ( hb_IsNumeric(arg1) )
      nAt := arg1
   ELSEIF ( hb_IsString(arg1) )
      nAt := ::__indexOf(arg1)
   ENDIF
   IF ( nAt > 0 .AND. nAt <= LEN(::aItems) )
      IF ( ::aItems[nAt]:installed() )
         HqlQApplication:removeTranslator( ::aItems[nAt]:translator() ) //  HbQt_454 problem: doesn't returns boolean value
      ENDIF
      hb_Adel( ::aItems, nAt, .T. )
   ENDIF
RETURN self

/*!

 \brief Install translator[s]
 \param(IN)
 \return self

*/
METHOD install() CLASS hql_translDb
   ::__removeTranslators()
   ::__installTranslators()
RETURN self

/*!

 \brief Remove translator[s]
 \param(IN)
 \return self

*/
METHOD remove() CLASS hql_translDb
   ::__removeTranslators()
RETURN self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] find item position for given prefix
 \param(IN) string
 \return numeric

*/
METHOD __indexOf( cPrefix ) CLASS hql_translDb
   LOCAL item
   LOCAL nAt := 0
   FOR EACH item IN ::aItems
      IF ( item:prefix() == cPrefix )
         nAt := item:__enumIndex()
         EXIT
      ENDIF
   NEXT
RETURN nAt

/*!

 \brief [PROTECTED] Install translations
 \param(IN) string
 \return numeric

*/
METHOD __installTranslators() CLASS hql_translDb
   LOCAL item
   LOCAL oQlocale := HqlFw:QtLocale()  /*HqlQLocale*/
   LOCAL cPath, cFile, oFileinfo
   LOCAL oTranslator

   FOR EACH item IN ::aItems
      IF ( item:isValid() .AND. !item:installed() )
         // e.g. "qt_" + "it_IT" + ".qm" , "myapp_" + "en_EN" + ".qm"
         cFile := item:prefix() + oQlocale:name() + ".qm" // .qm is Qt compiled language
         cPath := item:path()
         IF ( EMPTY(cPath) )
            cPath := HqlQLibraryInfo:location( QLibraryInfo_TranslationsPath )  // trick to get Qt default path BUT only when qt.conf exists!!!
         ENDIF

         oFileinfo := QFileInfo()
         oFileinfo:setFile( QDir(cPath), cFile )
         oTranslator := QTranslator()

         IF ( oTranslator:load( oFileInfo:fileName(), oFileInfo:path() ) )
            item:setTranslator( oTranslator )
            HqlQApplication:installTranslator( oTranslator ) //  HbQt_454 problem: doesn't returns boolean value
         ENDIF
      ENDIF
   NEXT

RETURN NIL

/*!

 \brief [PROTECTED] find item position for given prefix
 \param(IN) string
 \return numeric

*/
METHOD __removeTranslators() CLASS hql_translDb
   LOCAL item

   FOR EACH item IN ::aItems
      IF ( item:installed() )
         HqlQApplication:removeTranslator( item:translator() ) //  HbQt_454 problem: doesn't returns boolean value
      ENDIF
      item:setTranslator( NIL )
   NEXT

RETURN NIL

// ==================== HIDDEN section ====================

////////////////////////////////////////  ====  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

CLASS hql_translItem STATIC

   EXPORTED:
   METHOD init
   METHOD installed                       INLINE ( hb_IsObject(::oTranslator) )
   METHOD isValid                         INLINE ( LEN(ALLTRIM(::cPrefix)) > 0 )
   METHOD path                            INLINE ::cPath
   METHOD prefix                          INLINE ::cPrefix
   METHOD setPath
   METHOD setPrefix
   METHOD setTranslator
   METHOD translator                      INLINE ::oTranslator

   PROTECTED:
   VAR cPath                              INIT ""
   VAR cPrefix                            INIT ""
   VAR oTranslator                        INIT NIL

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance
 \param(IN) ...
 \return self

*/
METHOD init( cPrefix, cPath ) CLASS hql_translItem
   ::setPrefix( cPrefix )
   ::setPath( cPath )
RETURN self

/*!

 \brief set path
 \param(IN) string
 \return self

*/
METHOD setPath( arg1 ) CLASS hql_translItem
   IF ( hb_IsString(arg1) )
      ::cPath := arg1
   ENDIF
RETURN self

/*!

 \brief set prefix
 \param(IN) string
 \return self

*/
METHOD setPrefix( arg1 ) CLASS hql_translItem
   IF ( hb_IsString(arg1) )
      ::cPrefix := arg1
   ENDIF
RETURN self

/*!

 \brief set translator
 \param(IN) QTranslator
 \return self

*/
METHOD setTranslator( arg1 ) CLASS hql_translItem
   IF ( hql_IsDerived(arg1, "QTranslator") )
      ::oTranslator := arg1
   ELSEIF ( arg1 == NIL )
      ::oTranslator := NIL
   ENDIF
RETURN self
