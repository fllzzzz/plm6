
import { meteFmtByBasicClass, getBasicClassUnit } from '@/utils/other'

function preparesCustomSummary({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row = meteFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      precision: 5,
      field: ['value'],
      unit: ['unit'],
      returnNewObj: true,
      isNum: true
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
    row = meteFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      field: ['value', 'returnValue'],
      returnNewObj: true,
      isNum: true
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
    row = meteFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      field: ['value'],
      returnNewObj: true,
      isNum: true
    })
    row.checkUnit = getBasicClassUnit(row.basicClass, false)
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
