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

 \brief Returns a new hql_picture object instance
 \param(IN) ...
 \return object

*/
FUNCTION hqlPicture( ... )
RETURN hql_picture():new( ... )

/*!

 \brief define hql_picture class

*/
CLASS hql_picture INHERIT hql_label

   EXPORTED:
   METHOD init
   METHOD hqlAspectRatioMode              INLINE ::nHqlAspectRatioMode
   METHOD hqlEmptyPixmap
   METHOD hqlNullPixmap
   METHOD hqlPicSize
   METHOD hqlSetAspectRatioMode
   METHOD hqlSetPicSize
   METHOD hqlSetTransformMode
   METHOD hqlTransformMode                INLINE ::nHqlTransformMode
   METHOD pixmap                                            // override Qt method
   METHOD setPixmap                                         // override Qt method
   METHOD setScaledContents                                 // override Qt method

   PROTECTED:
   VAR oHqlPicSize                        INIT NIL
   VAR oHqlPixmap                         INIT NIL
   VAR nHqlAspectRatioMode                INIT Qt_IgnoreAspectRatio     // default value see scaledContents
   VAR nHqlTransformMode                  INIT Qt_SmoothTransformation  // default value see scaledContents
   METHOD __hqlCleaner
   METHOD __hqlConnect
   METHOD __hqlScaledPixmap
   METHOD __hqlSetAspectRatioMode
   METHOD __hqlSetPicSize
   METHOD __hqlSetTransformMode
   METHOD __hqlSetPixmap
   SIGNAL __hql_QResize

   HIDDEN:

ENDCLASS

/*!

 \brief initialize object instance for given [name], [Qt flags]
 \param(IN) string, ...
 \return self

*/
METHOD init( ... ) CLASS hql_picture

   ::hql_label:init( ... )

   ::setAlignment( Qt_AlignCenter ) // by default in center

   ::oHqlPixmap := ::hqlNullPixmap()   // to mimic Qt setPixamp() and pixmap() (they returns original pixmap)
                                       // we need a var to keep original pixmap

   ::setPixmap( ::oHqlPixmap )         // to bypass HbQt ::pixmap() crash when Qt pixmap() returns 0 (Qt default);
                                       // it's a hbqt/qtcore/hbqt_bind.cpp / hbqt_bindGetHbObject problem
                                       // When QLable.cpp returns a null pointer HbQt performs
                                       // hbqt_bindGetHbObject( .... new QPixmap( *( p->pixmap() ) ) ...
                                       // hbqt_bindGetHbObject crash because it doesn't return a valid HbQt object pointer
                                       // and hb_itemreturnrelease(...) need a valid pointer to update stack

RETURN Self

/*!

 \brief it returns a transparent pixmap (default) or a colored pixmap if QColor given
 \param[in] none
 \return QPixmap

*/
METHOD hqlEmptyPixmap( ... ) CLASS hql_picture

   LOCAL oQimage
   LOCAL oQpainter
   LOCAL oQcolor

   oQimage := QImage( ::width(), ::height(), QImage_Format_ARGB32 )

   IF hql_IsClass( hb_Pvalue(1), "Qcolor" )
      oQcolor := hb_Pvalue(1)
   ELSE
      oQcolor := QColor( 0, 0, 0, 0 )
   ENDIF

   oQpainter := QPainter()

   oQpainter:begin( oQimage )
   oQpainter:fillRect( oQimage:rect(), oQcolor )
   oQpainter:end()

RETURN QPixmap():fromImage( oQimage )

/*!

 \brief it returns a NULL pixmap
 \param[in] none
 \return QPixmap

*/
METHOD hqlNullPixmap() CLASS hql_picture
RETURN QPixmap():fromImage( QImage( 0, 0, QImage_Format_ARGB32 ) )

/*!

 \brief returns fixed pixmap size or container size
 \param[in] none
 \return QPixmap

*/
METHOD hqlPicSize() CLASS hql_picture

   IF ( hql_IsDerived(::oHqlPicSize, "Qsize") )
      RETURN ::oHqlPicSize
   END IF

RETURN ::size()

/*!

 \brief set aspect ratio
 \param[in] numeric value
 \return Self

*/
METHOD hqlSetAspectRatioMode( ... ) CLASS hql_picture

   IF ::__hqlSetAspectRatioMode( ... )
      ::__hqlSetPixmap( ::__hqlScaledPixmap( /*oSize*/ ) )
   ENDIF

RETURN Self

/*!

 \brief set fixed size for pixmap
 \param[in] QSize object || width,height || NIL
 \return Self

*/
METHOD hqlSetPicSize( ... ) CLASS hql_picture

   IF ::__hqlSetPicSize( ... )
      ::__hqlSetPixmap( ::__hqlScaledPixmap( /*oSize*/ ) )
   ENDIF

RETURN Self

/*!

 \brief set transformation mode
 \param[in] numeric value
 \return Self

*/
METHOD hqlSetTransformMode( ... ) CLASS hql_picture

   IF ::__hqlSetTransformMode( ... )
      ::__hqlSetPixmap( ::__hqlScaledPixmap( /*oSize*/ ) )
   ENDIF

RETURN Self

/*!

 \brief it returns pixmap: we override to mimic Qt standard where it returns the original pixmap
 \param[in] none
 \return QPixmap

*/
METHOD pixmap() CLASS hql_picture
RETURN ::oHqlPixmap

/*!

 \brief it returns pixmap: we override to mimic Qt standard where it returns the original pixmap
 \param[in] none
 \return QPixmap

*/
METHOD setPixmap( ... ) CLASS hql_picture

   IF PCOUNT() > 0

      ::oHqlPixmap := QPixmap( ... )

      SWITCH PCOUNT()
      CASE 5   // pixmap, width, height, aspectRatio, transformMode
         ::__hqlSetPicSize( hb_Pvalue(2), hb_Pvalue(3) )
         ::__hqlSetAspectRatioMode( hb_Pvalue(4) )
         ::__hqlSetTransformMode( hb_Pvalue(5) )
         EXIT

      CASE 4   // pixmap, size, aspectRatio, transformMode || pixmap, width, height, aspectRatio
         IF ( hql_IsDerived(hb_Pvalue(2), "Qsize") )
            ::__hqlSetPicSize( hb_Pvalue(2) )
            ::__hqlSetAspectRatioMode( hb_Pvalue(3) )
            ::__hqlSetTransformMode( hb_Pvalue(4) )
         ELSE
            ::__hqlSetPicSize( hb_Pvalue(2), hb_Pvalue(3) )
            ::__hqlSetAspectRatioMode( hb_Pvalue(4) )
         END IF
         EXIT

      CASE 3   // pixmap, size, aspectRatio || pixmap, width, height
         IF ( hql_IsDerived(hb_Pvalue(2), "Qsize") )
            ::__hqlSetPicSize( hb_Pvalue(2) )
            ::__hqlSetAspectRatioMode( hb_Pvalue(3) )
         ELSE
            ::__hqlSetPicSize( hb_Pvalue(2), hb_Pvalue(3) )
         END IF
         EXIT

      CASE 2   // pixmap, size
         IF ( hql_IsDerived(hb_Pvalue(2), "Qsize") )
            ::__hqlSetPicSize( hb_Pvalue(2) )
         ENDIF
         EXIT

      END SWITCH

      ::__hqlSetPixmap( ::__hqlScaledPixmap( /*oSize*/ ) )

   ENDIF

RETURN Self

/*!

 \brief change scaledContents flag
 \param[in] boolean
 \return Self

*/
METHOD setScaledContents( ... ) CLASS hql_picture

   ::hql_label:setScaledContents( ... )

   // specialized
   ::__hqlSetPixmap( ::__hqlScaledPixmap( /*oSize*/ ) )

RETURN Self

// ==================== PROTECTED section ====================

/*!

 \brief [PROTECTED] cleaner
 \param[in] ...
 \return NIL

*/
METHOD __hqlCleaner() CLASS hql_picture
   ::oHqlPicSize := NIL
   ::oHqlPixmap := NIL
   ::hql_label:__hqlCleaner()
RETURN NIL

/*!

 \brief [PROTECTED] returns true if connected else false
 \param[in] none
 \return boolean

*/
METHOD __hqlConnect() CLASS hql_picture

   IF ::hql_label:__hqlConnect() .AND. ;
      ::connect( QEvent_Resize, { |oEvent| ::__hql_QResize(oEvent) } )
      RETURN .T.
   ENDIF

RETURN .F.

/*!

 \brief [PROTECTED] return scaled pixmap; if argument oSize is given will be used else label size will be used only if fixed size not settled

*/
METHOD __hqlScaledPixmap( oSize ) CLASS hql_picture

   // user requested Qt scaling
   IF ::hasScaledContents()
      RETURN ::oHqlPixmap
   END IF

   // when not given valid size, label widget size assumed
   IF ( !hql_IsDerived(oSize, "Qsize") )
      oSize := ::size()
   END IF

   IF ( hql_IsDerived(::oHqlPicSize, "Qsize") )
      oSize := ::oHqlPicSize:boundedTo( oSize )
   END IF

   // find size
   oSize := ::oHqlPixmap:size:boundedTo( oSize )

RETURN ::oHqlPixmap:scaled( oSize, ::nHqlAspectRatioMode, ::nHqlTransformMode )

/*!

 \brief [PROTECTED] set fixed size of pixmap
 \param[in] QSize object || width,height || NIL
 \return Self

*/
METHOD __hqlSetPicSize( ... ) CLASS hql_picture

   LOCAL lUpdateImage := .F.

   IF ( PCOUNT() == 1 .AND. hql_IsDerived(hb_Pvalue(1), "Qsize") .AND. hb_Pvalue(1):isValid() )
      ::oHqlPicSize := QSize( hb_Pvalue(1) )
      lUpdateImage := .T.
   ELSEIF ( PCOUNT() == 2 .AND.  hb_IsNumeric( hb_Pvalue(1) ) .AND. hb_IsNumeric( hb_Pvalue(2) ) .AND. QSize( hb_Pvalue(1), hb_Pvalue(2) ):isValid() )
      ::oHqlPicSize := QSize( hb_Pvalue(1), hb_Pvalue(2) )
      lUpdateImage := .T.
   ELSEIF ( PCOUNT() == 1 .AND.  hb_Pvalue(1) == NIL )
      ::oHqlPicSize := NIL
      lUpdateImage := .T.
   ENDIF

RETURN lUpdateImage

/*!

 \brief [PROTECTED] set pixmap helper

*/
METHOD __hqlSetPixmap( oPixmap ) CLASS hql_picture
   ::hql_label:setPixmap( oPixmap )
RETURN NIL

/*!

 \brief [PROTECTED] set aspect ratio
 \param[in] numeric value
 \return Self

*/
METHOD __hqlSetAspectRatioMode( ... ) CLASS hql_picture

   LOCAL lUpdateImage := .F.

   IF ( hb_IsNumeric( hb_Pvalue(1) ) .AND. hb_Pvalue(1) >= Qt_IgnoreAspectRatio .AND. hb_Pvalue(1) <= Qt_KeepAspectRatioByExpanding )
      ::nHqlAspectRatioMode := hb_Pvalue(1)
      lUpdateImage := .T.
   ENDIF

RETURN lUpdateImage

/*!

 \brief [PROTECTED] set transformation mode
 \param[in] numeric value
 \return Self

*/
METHOD __hqlSetTransformMode( ... ) CLASS hql_picture

   LOCAL lUpdateImage := .F.

   IF ( hb_IsNumeric( hb_Pvalue(1) ) .AND. hb_Pvalue(1) >= Qt_FastTransformation .AND. hb_Pvalue(1) <= Qt_SmoothTransformation )
      ::nHqlTransformMode := hb_Pvalue(1)
      lUpdateImage := .T.
   ENDIF

RETURN lUpdateImage

// ==================== SLOTS/EVENTS section ====================

/*!

 \brief [PROTECTED] handle event/signal
 \param[in] ... based on event/signal
 \return false ==> .F. means don't stop event hanlder, else .T. STOP see Harbour Changelog

*/
SIGNAL __hql_QResize(oEvent) CLASS hql_picture

   ::__hqlSetPixmap( ::__hqlScaledPixmap( oEvent:size() ) )

RETURN .F.

// ==================== HIDDEN section ====================
