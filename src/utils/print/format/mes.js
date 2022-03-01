
// import { meteFmtByMaterialType, meteFmtByMaterialListType } from '@/utils/other'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useWageQuotaUnit from '@compos/mes/use-wage-quota-unit'
import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'

// 结构生产线的量和工序完成数
function productionLineMete({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.processSequence = row.processSummaryDetailsDOList.map((o) => {
      return `【${o.name}│${
        o.completeQuantity === row.taskQuantity ? '√' : o.completeQuantity || 0
      }】`
    }).join('→')

    const unitObj = useProductSummaryMeteUnit({ productType: row.productType, l_unit: 'mm', w_unit: 'kg', isSingle: true }) || {}
    row.mete = useProductMeteConvert({
      productType: row.productType,
      weight: { num: row.netWeight },
      length: { num: row.length, to: unitObj.unit, dp: unitObj.dp }
    })
    if (row.mete) {
      row.mete += unitObj.unit
    }
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 结构工序的量和未完成数
function processMete({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.unCompleteQuantity = row.taskQuantity - row.completeQuantity
    const unitObj = useProductSummaryMeteUnit({ productType: row.productType, w_unit: 'kg' })
    row.mete = useProductMeteConvert({
      productType: row.productType,
      weight: { num: row.completeNetWeight },
      length: { num: row.completeLength, to: unitObj.unit, dp: unitObj.dp }
    })
    if (row.mete) {
      row.mete += unitObj.unit
    }
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 计件制生产量单位
function meteUnit({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const unitObj = row.productType && useProductSummaryMeteUnit({ productType: row.productType })
    // 核算量
    row.checkMete = row.wageQuotaType && useWageQuotaMeteConvert({
      length: row.mate,
      weight: row.mate,
      surfaceArea: row.mate,
      wageQuotaType: row.wageQuotaType,
      showUnit: true
    }).convertMete
    // 生产量
    row.productMete = row.mate + unitObj?.unit
    // 长度/重量
    row.mete = row.productType && useProductMeteConvert({
      productType: row.productType,
      length: { num: row.length * row.quantity, to: unitObj.unit, dp: unitObj.dp },
      weight: { num: row.netWeight * row.quantity, to: unitObj.unit, dp: unitObj.dp }
    })
    if (row.mete) {
      row.mete += unitObj?.unit
    }
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 未生产量
function unProducedMete({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.unProducedQuantity = row.taskQuantity - row.completeQuantity - row.inProductionQuantity
    // 构件
    if (Object.prototype.hasOwnProperty.call(row, 'taskNetWeight')) {
      row.unProducedMete = row.taskNetWeight - row.completeNetWeight - row.inProductionNetWeight
    }
    // 围护
    if (Object.prototype.hasOwnProperty.call(row, 'taskLength')) {
      row.unProducedMete = row.taskLength - row.completeLength - row.inProductionLength
    }
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 未完成量
function unCompleteMete({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const unitObj = useProductSummaryMeteUnit({ productType: row.productType, w_unit: 'kg' })
    row.unCompleteQuantity = row.taskQuantity - row.completeQuantity
    row.unCompleteMete = useProductMeteConvert({
      productType: row.productType,
      weight: { num: row.taskNetWeight - row.completeNetWeight },
      length: { num: row.taskLength - row.completeLength, to: unitObj.unit, dp: unitObj.dp }
    })
    if (row.unCompleteMete) {
      row.unCompleteMete += unitObj.unit
    }
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 项目汇总
function projectSummary({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const unitObj = useProductSummaryMeteUnit({ productType: row.productType, w_unit: 'kg', isSingle: true })
    row.mete = useProductMeteConvert({
      productType: row.productType,
      length: { num: row.length, to: unitObj.unit, dp: unitObj.dp },
      weight: { num: row.netWeight, to: unitObj.unit, dp: unitObj.dp }
    })
    row.completeMete = useProductMeteConvert({
      productType: row.productType,
      length: { num: row.length * row.completeQuantity, to: unitObj.unit, dp: unitObj.dp },
      weight: { num: row.netWeight * row.completeQuantity, to: unitObj.unit, dp: unitObj.dp }
    })
    row.shipMete = useProductMeteConvert({
      productType: row.productType,
      length: { num: row.length * row.sendQuantity, to: unitObj.unit, dp: unitObj.dp },
      weight: { num: row.netWeight * row.sendQuantity, to: unitObj.unit, dp: unitObj.dp }
    })
    row.sendQuantity = row.sendQuantity || 0
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 工资生产量
function wageProducedMete({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const unitObj = useProductSummaryMeteUnit({ productType: row.productType, w_unit: 'kg' })
    row.taskMete = useProductMeteConvert({
      productType: row.productType,
      weight: { num: row.taskNetWeight },
      length: { num: row.taskLength, to: unitObj.unit, dp: unitObj.dp }
    })
    row.completeMete = useProductMeteConvert({
      productType: row.productType,
      weight: { num: row.completeNetWeight },
      length: { num: row.completeLength, to: unitObj.unit, dp: unitObj.dp }
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

// 工资详情完成量单位
function wageCompleteMete({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const unitObj = useProductSummaryMeteUnit({ productType: row.productType })
    // 核算量
    const wageQuotaType = useWageQuotaUnit({ wageQuotaType: row.wageQuotaType })
    row.checkMete = row.mate + wageQuotaType?.meteUnit
    // 完成量
    row.completeMete = useProductMeteConvert({
      productType: row.productType,
      weight: { num: row.completeNetWeight },
      length: { num: row.completeLength, to: unitObj.unit, dp: unitObj.dp }
    })
    if (row.completeMete) {
      row.completeMete += unitObj.unit
    }
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 表面积
function surfaceArea({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.loss = row.loss ? row.loss * 100 : row.loss
    row.volumeSolids = row.volumeSolids ? row.volumeSolids * 100 : row.volumeSolids
    row.surfaceArea = convertUnits(row.surfaceArea, 'mm²', '㎡', DP.COM_AREA__M2)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

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
    // row = meteFmtByMaterialType(config)
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
    // row = meteFmtByMaterialListType(config)
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
    row = numFmtByBasicClass({
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

export default {
  productionLineMete,
  processMete,
  meteUnit,
  surfaceArea,
  projectSummary,
  unProducedMete,
  unCompleteMete,
  wageProducedMete,
  wageCompleteMete,
  materialType,
  materialListType,
  steelDosageFormat
}
