import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

function preparesCustomSummary({ header, table = [], footer, qrCode }) {
  const _table = table.map((row) => {
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

async function dataFormat({ header, table = [], footer, qrCode }) {
  await setSpecInfoToList(table)
  await numFmtByBasicClass(table)
  return {
    header,
    table,
    footer,
    qrCode
  }
}

function checkUnitFormat({ header, table = [], footer, qrCode }) {
  const _table = table.map((row) => {
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
  dataFormat
}
