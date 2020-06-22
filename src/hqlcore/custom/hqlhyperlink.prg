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

 \brief Returns a new hql_hyperLink object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlHyperLink( ... )
RETURN hql_hyperLink():new( ... )

/*!

 \brief define hql_hyperLink class

*/
CLASS hql_hyperLink INHERIT hql_label

   EXPORTED:
   METHOD init
   METHOD hqlHttp
   METHOD hqlMailTo
   METHOD hqlOnLinkActivated
   METHOD hqlRunAct

   PROTECTED:
   VAR bHqlLinkActivated                  INIT NIL
   SIGNAL __hql_QLinkActivated

   HIDDEN:

ENDCLASS

/*!

 \brief Initialize a new object instance
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_hyperLink

   ::hql_label:init( ... )

   ::setTextFormat( Qt_RichText )
   // note by Qt:
   // The textInteractionFlags set on the label need to include either LinksAccessibleByMouse or LinksAccessibleByKeyboard.
   ::setTextInteractionFlags( Qt_TextBrowserInteraction )
   // note by Qt:
   // Specifies whether QLabel should automatically open links using QDesktopServices::openUrl() instead of emitting the linkActivated() signal.
   ::setOpenExternalLinks( .T. )  // most important to run client

RETURN self

/*!

 \brief open external link protocol http | https
 \param(IN) strings href [, text to show]
 \return self

*/
METHOD hqlHttp( cHref, cTextToShow ) CLASS hql_hyperLink

   cHref := hb_DefaultValue(cHref, "")
   cTextToShow := hb_DefaultValue(cTextToShow, "")

   IF ( LEN(ALLTRIM(cHref)) > 0 )

      IF ( hb_AtI( "http://", cHref, 1, 7 ) > 0 ) .OR. ( hb_AtI( "http:", cHref, 1, 5 ) > 0 ) .OR. ;
         ( hb_AtI( "https://", cHref, 1, 8 ) > 0 ) .OR. ( hb_AtI( "https:", cHref, 1, 6 ) > 0 )
         // nothing to do
      ELSE
         cHref := "http:" + cHref
      ENDIF

      IF ( LEN(ALLTRIM(cTextToShow)) == 0 )
         cTextToShow := cHref
      ENDIF

      ::setText( "<a href='" + cHref + "'>" + cTextToShow + "</a>" )

   ENDIF

RETURN Self

/*!

 \brief open external link protocol http | https
 \param(IN) strings href [, text to show]
 \return self

*/
METHOD hqlMailTo( cHref, cTextToShow ) CLASS hql_hyperLink

   cHref := hb_DefaultValue(cHref, "")
   cTextToShow := hb_DefaultValue(cTextToShow, "")

   IF ( LEN(ALLTRIM(cHref)) > 0 )

      IF ( hb_AtI( "mailto://", cHref, 1, 9 ) > 0 ) .OR. ( hb_AtI( "mailto:", cHref, 1, 7 ) > 0 )
         // nothing to do
      ELSE
         cHref := "mailto:" + cHref
      ENDIF

      IF ( LEN(ALLTRIM(cTextToShow)) == 0 )
         cTextToShow := cHref
      ENDIF

      ::setText( "<a href='" + cHref + "'>" + cTextToShow + "</a>" )

   ENDIF

RETURN Self

/*!

 \brief set/get block
 \param(IN) block | NIL
 \return Self

*/
METHOD hqlOnLinkActivated( arg1 ) CLASS hql_hyperLink
   IF ( PCOUNT() == 1 .AND. ( hb_IsEvalItem(arg1 ) .OR. arg1 == NIL ) )
      ::bHqlLinkActivated := arg1
      IF ( hb_IsEvalItem(::bHqlLinkActivated) )
         ::connect( "linkActivated(QString)", { |cString| ::__hql_QLinkActivated(cString) } )
      ELSE
         ::disconnect( "linkActivated(QString)" )
      ENDIF
   ENDIF
RETURN self

/*!

 \brief to use label as a button with hyper text link
 \param(IN) strings href [, text to show]
 \return self

*/
METHOD hqlRunAct( cHref, cTextToShow ) CLASS hql_hyperLink

   cHref := hb_DefaultValue(cHref, "")
   cTextToShow := hb_DefaultValue(cTextToShow, "")

   IF ( LEN(ALLTRIM(cHref)) > 0 )
      IF ( LEN(ALLTRIM(cTextToShow)) == 0 )
         cTextToShow := cHref
      ENDIF

      ::setText( "<a href='" + cHref + "'>" + cTextToShow + "</a>" )
      ::setOpenExternalLinks( .F. )  // most important to avoids running of external program

   ENDIF

RETURN Self

// ==================== PROTECTED section ====================

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] signal/event handler
   false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog
 \param(IN) ...
 \return bool

*/
SIGNAL __hql_QLinkActivated( cString ) CLASS hql_hyperLink
   IF hb_IsEvalItem( ::bHqlLinkActivated )
      EVAL( ::bHqlLinkActivated, cString, Self )  // Self always as last
   ENDIF
RETURN .F.

// ==================== HIDDEN section ====================
