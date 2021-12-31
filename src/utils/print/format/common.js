
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

function meteWithUnit({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row = numFmtByBasicClass({
      data: row,
      enclosureSettlementType: header && header.enclosureSettlementType,
      basicClass: row.basicClass,
      precision: 5,
      returnNewObj: true,
      toNum: true
    })
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}
function filter({ header, table = [], footer, qrCode }) {
  const _table = table.filter(row => {
    return !(row.storageMete === 0 && row.beginInventory === 0 && row.currentInventory === 0 && row.outboundMete === 0)
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

export default { meteWithUnit, filter }
