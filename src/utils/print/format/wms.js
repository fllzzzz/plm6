
// import { getBasicClassUnit } from '@/utils/other'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

function preparesCustomSummary({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row = numFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      precision: 5,
      field: ['value'],
      unit: ['unit'],
      returnNewObj: true,
      toNum: true
    })
    return row
  })
  return {
    header,
    table: _table,
    footer,
    qrCode
  }
}

function valueFormat({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.returnValue = row.returnTotalWeight || row.returnTotalLength || row.returnTotal || 0
    row = numFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      field: ['value', 'returnValue'],
      returnNewObj: true,
      toNum: true
    })
    return row
  })
  return {
    header,
    table: _table,
    footer,
    qrCode
  }
}

function checkUnitFormat({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row = numFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      field: ['value'],
      returnNewObj: true,
      toNum: true
    })
    // row.checkUnit = getBasicClassUnit(row.basicClass, false)
    return row
  })
  return {
    header,
    table: _table,
    footer,
    qrCode
  }
}

export default {
  preparesCustomSummary,
  checkUnitFormat,
  valueFormat
}
