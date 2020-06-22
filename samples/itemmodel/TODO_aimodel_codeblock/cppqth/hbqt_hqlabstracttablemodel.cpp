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
/*

   From Qt https://github.com/qt/qtbase/blob/5.9/src/corelib/itemmodels/qabstractitemmodel.cpp
    When subclassing QAbstractItemModel, at the very least you must implement
    index(), parent(), rowCount(), columnCount(), and data(). These functions
    are used in all read-only models, and form the basis of editable models.
    You can also reimplement hasChildren() to provide special behavior for
    models where the implementation of rowCount() is expensive. This makes it
    possible for models to restrict the amount of data requested by views, and
    can be used as a way to implement lazy population of model data.
    To enable editing in your model, you must also implement setData(), and
    reimplement flags() to ensure that \c ItemIsEditable is returned.  You can
    also reimplement headerData() and setHeaderData() to control the way the
    headers for your model are presented.

It is not necessary to support every role defined in Qt::ItemDataRole.
    Depending on the type of data contained within a model, it may only be
    useful to implement the data() function to return valid information for
    some of the more common roles. Most models provide at least a textual
    representation of item data for the Qt::DisplayRole, and well-behaved
    models should also provide valid information for the Qt::ToolTipRole and
    Qt::WhatsThisRole. Supporting these roles enables models to be used with
    standard Qt views. However, for some models that handle highly-specialized
    data, it may be appropriate to provide data only for user-defined roles.
    Models that provide interfaces to resizable data structures can provide
    implementations of insertRows(), removeRows(), insertColumns(),and
    removeColumns(). When implementing these functions, it is important to
    notify any connected views about changes to the model's dimensions both
    \e before and \e after they occur:
    \list
        \li An insertRows() implementation must call beginInsertRows() \e before
           inserting new rows into the data structure, and endInsertRows()
           \e{immediately afterwards}.
        \li An insertColumns() implementation must call beginInsertColumns()
           \e before inserting new columns into the data structure, and
           endInsertColumns() \e{immediately afterwards}.
        \li A removeRows() implementation must call beginRemoveRows() \e before
           the rows are removed from the data structure, and endRemoveRows()
           \e{immediately afterwards}.
        \li A removeColumns() implementation must call beginRemoveColumns()
           \e before the columns are removed from the data structure, and
           endRemoveColumns() \e{immediately afterwards}.
    \endlist

*/
#include "hbqt.h"

#include "hbapiitm.h"
#include "hbvm.h"
#include "hbapicls.h"
#include "hbapierr.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hqlabstracttablemodel.h"

#include <QtGui/QBrush>
#include <QtGui/QColor>
#include <QtGui/QIcon>
#include <QtGui/QFont>
#include <QtGui/QPixmap>
#include <QtCore/QFlags>
#include <QtCore/QModelIndex>
#include <QtCore/QSize>
#include <QtCore/QDate>
#include <QtCore/QDateTime>
#include <QtCore/QString>
#include <QtCore/QVariant>

#if QT_VERSION <= 0x040900
#include <QtGui/QWidget>
#else
#include <QtWidgets/QWidget>
#endif

static QVariant hbqt_fetchData( PHB_ITEM block, int type, int role, int par1, int par2 )
{
   QVariant vv;

   if( hb_vmRequestReenter() && block )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, type );
      PHB_ITEM p1 = hb_itemPutNI( NULL, role );
      PHB_ITEM p2 = hb_itemPutNI( NULL, par1 );
      PHB_ITEM p3 = hb_itemPutNI( NULL, par2 );

      PHB_ITEM ret = hb_itemNew( hb_vmEvalBlockV( block, 4, p0, p1, p2, p3 ) );

      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );

      if( hb_itemType( ret ) & HB_IT_STRING )
      {
         void * pText01 = NULL;
         vv = hb_itemGetStrUTF8( ret, &pText01, NULL );
         hb_strfree( pText01 );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ s = %s ]", hb_itemGetCPtr( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_LOGICAL )
      {
         vv = hb_itemGetL( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ l = %i ]", hb_itemGetL( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_DOUBLE  )
      {
         vv = hb_itemGetND( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ d = %f ]", hb_itemGetND( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_NUMERIC )
      {
         vv = hb_itemGetNI( ret );
         HB_TRACE( HB_TR_DEBUG, ( "   fetchData[ n = %i ]", hb_itemGetNI( ret ) ) );
      }
      else if( hb_itemType( ret ) & HB_IT_OBJECT )
      {
         void * p = hbqt_get_ptr( ret );

         if( hbqt_obj_isDerivedFrom( ret, "QBRUSH" ) )
            vv = * ( ( QBrush * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( ret, "QCOLOR" ) )
            vv = * ( ( QColor * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( ret, "QICON" ) )
            vv = * ( ( QIcon * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( ret, "QSIZE" ) )
            vv = * ( ( QSize * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( ret, "QFONT" ) )
            vv = * ( ( QFont * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( ret, "QPIXMAP" ) )
            vv = * ( ( QPixmap * ) ( p ) );
         else if( hbqt_obj_isDerivedFrom( ret, "QVARIANT" ) )
            vv = * ( ( QVariant * ) ( p ) );
      }

      hb_itemRelease( ret );
      hb_vmRequestRestore();
   }

   return vv;
}

static bool hbqt_setData( PHB_ITEM block, int type, int role, int iCol, int iRow, const QVariant & qVariantObj )
{
   bool bRet = false;

   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, type );
      PHB_ITEM p1 = hb_itemPutNI( NULL, role );
      PHB_ITEM p2 = hb_itemPutNI( NULL, iCol );    // section for setHeaderData
      PHB_ITEM p3 = hb_itemPutNI( NULL, iRow );    // oriontation for setHeaderData
      PHB_ITEM p4;

      switch( qVariantObj.type() )
      {
         case QVariant::Bool:
         {
            p4 = hb_itemPutL( NULL, qVariantObj.toBool() );
         }
         break;

         case QVariant::Char:
         case QVariant::String:
         {
            QString qS;
            qS = qVariantObj.toString();
            p4 = hb_itemPutStrLenUTF8( NULL, qS.toUtf8().constData(), qS.size() );
         }
         break;

         case QVariant::Date:
         {
            QDate qD;
            qD = qVariantObj.toDate();
            p4 = hb_itemPutDL( NULL, qD.toJulianDay() );
         }
         break;
#if QT_VERSION >= 0x040900
         case QVariant::DateTime:
         {
            QDateTime qDT;
            QDate qD;
            qDT = qVariantObj.toDateTime();
            qD = qDT.date();
            p4 = hb_itemPutTDT( NULL, qD.toJulianDay(), qDT.time().msecsSinceStartOfDay());
         }
         break;
#endif
         case QVariant::LongLong:
         case QVariant::ULongLong:
         {
            p4 = hb_itemPutNLL( NULL, qVariantObj.toLongLong() );
         }
         break;

         case QVariant::Int:
         case QVariant::UInt:
         {
            p4 = hb_itemPutNI( NULL, qVariantObj.toInt() );
         }
         break;

         case QVariant::Double:
         {
            p4 = hb_itemPutND( NULL, qVariantObj.toDouble() );
         }
         break;
#if QT_VERSION >= 0x040900
         case QVariant::ModelIndex:
         {
            QModelIndex qMI;
            PHB_ITEM pRow;
            PHB_ITEM pCol;
            qMI = qVariantObj.toModelIndex();
            pRow = hb_itemPutNI( NULL, qMI.row() );
            pCol = hb_itemPutNI( NULL, qMI.column() );
            p4 = hb_itemArrayNew( 2 );
            hb_itemArrayPut( p4, 1, pRow );
            hb_itemArrayPut( p4, 2, pCol );
            hb_itemRelease( pCol );
            hb_itemRelease( pRow );
         }
         break;
#endif
         default:
            p4 = NULL;
      }

      if( p4 )
      {
         PHB_ITEM ret = hb_itemNew( hb_vmEvalBlockV( block, 5, p0, p1, p2, p3, p4 ) );
         hb_itemRelease( p4 );
         if( hb_itemType( ret ) & HB_IT_LOGICAL )
            bRet = hb_itemGetL( ret );
      }
      hb_itemRelease( p3 );
      hb_itemRelease( p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p0 );

      hb_vmRequestRestore();
   }

   return bRet;
}

HQLAbstractTableModel::HQLAbstractTableModel( QObject * parent ) : QAbstractTableModel( parent )
{
   block = NULL;
}

HQLAbstractTableModel::~HQLAbstractTableModel( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::~HQLAbstractTableModel()" ) );
   if( block ) {
      hb_itemRelease( block );
      block = NULL;
   }
}

/*!
   to set block
*/
void HQLAbstractTableModel::hbSetBlock( PHB_ITEM b )
{
   if( b ) {
      HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::hbSetBlock()" ) );
      if( block ) {
         hb_itemRelease( block );
      }
      block = hb_itemNew( b );
      hb_gcUnlock( block );
   }
}

/*!
   to clear block
*/
void HQLAbstractTableModel::hbClearBlock( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::hbClearBlock()" ) );
   if( block ) {
      hb_itemRelease( block );
      block = NULL;
   }
}

/*!
   I don't know why I must override this method
*/
QModelIndex HQLAbstractTableModel::index( int row, int column, const QModelIndex & /*parent*/ ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::index()" ) );
   if (row >= 0 && column >= 0)
      return createIndex( row, column );

   return QModelIndex();
}

/*!
   retrieve flags
*/
Qt::ItemFlags HQLAbstractTableModel::flags( const QModelIndex & index ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::flags()" ) );
   if ( index.isValid() ) {
      QVariant ret = hbqt_fetchData( block, HBQT_QAIM_flags, 0, index.column(), index.row() );
      if( ret.isValid() )
         HB_TRACE( HB_TR_DEBUG, ( "qui %i", ret.toInt() ) );
         return ( QFlags<Qt::ItemFlag> ) ret.toInt();

      return Qt::ItemIsSelectable|Qt::ItemIsEnabled|Qt::ItemNeverHasChildren; // see QAbstractTableModel::flags
   }

   return 0;
}

/*!
   retrieve data
*/
QVariant HQLAbstractTableModel::data( const QModelIndex & index, int role ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::data()" ) );
   if ( index.isValid() )
      return hbqt_fetchData( block, HBQT_QAIM_data, role, index.column(), index.row() );

   return QVariant();
}

/*!
   set data
*/
bool HQLAbstractTableModel::setData( const QModelIndex & index, const QVariant & value, int role )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::setData()" ) );
   if ( index.isValid() ) {
      bool ret = hbqt_setData( block, HBQT_QAIM_setData, role, index.column(), index.row(), value );
      if( ret ) {
         emit QAbstractTableModel::dataChanged( index, index );
         return ret;
      }
   }

   return false;
}

/*!
   retrieve header data
*/
QVariant HQLAbstractTableModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::headerData()" ) );
   return hbqt_fetchData( block, HBQT_QAIM_headerData, role, orientation, section );
}

/*!
   set header data
*/
bool HQLAbstractTableModel::setHeaderData( int section, Qt::Orientation orientation, const QVariant &value, int role )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::setHeaderData()" ) );
   bool ret = hbqt_setData( block, HBQT_QAIM_setHeaderData, role, section, orientation, value );
   if( ret ) {
      emit QAbstractTableModel::headerDataChanged( orientation, section, section );
      return ret;
   }

   return false;
}

/*!
   retrieve rowcount
*/
int HQLAbstractTableModel::rowCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::rowCount()" ) );
   return hbqt_fetchData( block, HBQT_QAIM_rowCount, 0, 0, 0 ).toInt();
}

/*!
   retrieve columncount
*/
int HQLAbstractTableModel::columnCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::columnCount()" ) );
   return hbqt_fetchData( block, HBQT_QAIM_columnCount, 0, 0, 0 ).toInt();
}

/*!
    Returns \c{true} if there is more data available for \a parent; otherwise
    returns \c{false}.
    The default implementation always returns \c{false}.
    If canFetchMore() returns \c true, the fetchMore() function should
    be called. This is the behavior of QAbstractItemView, for example.
    \sa fetchMore()
*/
bool HQLAbstractTableModel::canFetchMore( const QModelIndex & /* index */ ) const
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::canFetchMore()" ) );
   return hbqt_fetchData( block, HBQT_QAIM_canFetchMore, 0, 0, 0 ).toBool();
}

/*!
    Fetches any available data for the items with the parent specified by the
    \a parent index.
    Reimplement this if you are populating your model incrementally.
    The default implementation does nothing.
    \sa canFetchMore()

*/
void HQLAbstractTableModel::fetchMore( const QModelIndex & /*parent*/ )
{
   HB_TRACE( HB_TR_DEBUG, ( "HQLAbstractTableModel::fetchMore()" ) );
   hbqt_fetchData( block, HBQT_QAIM_fetchMore, 0, 0, 0 );
}

/* ================================= signals ================================= */

/*!
   reset()
*/
void HQLAbstractTableModel::reset()
{
   QAbstractTableModel::beginResetModel();
   QAbstractTableModel::endResetModel();

   emit QAbstractTableModel::layoutAboutToBeChanged();
   emit QAbstractTableModel::layoutChanged();
}


/*!
   EmitDataChanged( ... ) addon to emit dataChanged
*/
void HQLAbstractTableModel::EmitDataChanged( const QModelIndex & topLeft, const QModelIndex & bottomRight )
{
   QAbstractTableModel::dataChanged( topLeft, bottomRight );
}

/*!
   EmitHeaderDataChanged( ... ) addon to emit headerDataChanged
*/
void HQLAbstractTableModel::EmitHeaderDataChanged( Qt::Orientation orientation, int first, int last)
{
   QAbstractTableModel::headerDataChanged( orientation, first, last );
}

/*!
   EmitLayoutAboutToBeChanged() addon to emit layoutAboutToBeChanged
*/
void HQLAbstractTableModel::EmitLayoutAboutToBeChanged()
{
   QAbstractTableModel::layoutAboutToBeChanged( );
}

/*!
   EmitLayoutChanged() addon to emit layoutChanged
*/
void HQLAbstractTableModel::EmitLayoutChanged()
{
   QAbstractTableModel::layoutChanged( );
}

/*!
   beginInsertRows( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginInsertRows( const QModelIndex & parent, int first, int last )
{
   QAbstractTableModel::beginInsertRows( parent, first, last );
}

/*!
   endInsertRows() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endInsertRows()
{
   QAbstractTableModel::endInsertRows();
}

/*!
   beginRemoveRows( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginRemoveRows( const QModelIndex & parent, int first, int last )
{
   QAbstractTableModel::beginRemoveRows( parent, first, last );
}

/*!
   endRemoveRows() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endRemoveRows()
{
   QAbstractTableModel::endRemoveRows();
}

/*!
   beginInsertColumns( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginInsertColumns( const QModelIndex & parent, int first, int last)
{
   QAbstractTableModel::beginInsertColumns( parent, first, last );
}

/*!
   endInsertColumns() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endInsertColumns()
{
   QAbstractTableModel::endInsertColumns();
}

/*!
   beginRemoveColumns( ... ) addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginRemoveColumns( const QModelIndex & parent, int first, int last)
{
   QAbstractTableModel::beginRemoveColumns( parent, first, last );
}

/*!
   endRemoveColumns() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endRemoveColumns()
{
   QAbstractTableModel::endRemoveColumns();
}

/*!
   beginResetModel() addon not supplied by HbQt
*/
void HQLAbstractTableModel::beginResetModel()
{
   QAbstractTableModel::beginResetModel();
}

/*!
   endResetModel() addon not supplied by HbQt
*/
void HQLAbstractTableModel::endResetModel()
{
   QAbstractTableModel::endResetModel();
}

#endif
