
import { meteFmtByMaterialType, meteFmtByMaterialListType, meteFmtByBasicClass } from '@/utils/other'

function materialType({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const config = {
      data: row,
      type: row.materialType,
      precision: 5,
      returnNewObj: true,
      toNum: true
    }
    if (header) {
      config.enclosureSettlementType = header.enclosurePriceType
    }
    row = meteFmtByMaterialType(config)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

function materialListType({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const config = {
      data: row,
      type: row.materialListType,
      precision: 5,
      returnNewObj: true,
      toNum: true
    }
    if (header) {
      config.enclosureSettlementType = header.enclosurePriceType
    }
    row = meteFmtByMaterialListType(config)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

function steelDosageFormat({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row = meteFmtByBasicClass({
      data: row,
      basicClass: row.basicClass,
      field: ['modelMete', 'outboundMete', 'diff'],
      returnNewObj: true,
      toNum: true
    })
    row.diffRate = row.diffRate * 100
    return row
  })
  return {
    header,
    table: _table,
    footer,
    qrCode
  }
}

export default { materialType, materialListType, steelDosageFormat }
