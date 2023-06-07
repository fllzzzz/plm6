import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { matClsEnum } from '@/utils/enum/modules/classification'

// 处理物流商订单名称
function handleOrderName({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.orderName = row.name || row.supplierName
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理申购记录
async function handleRequisitionsRecord({ header, table = [], footer, qrCode }) {
  await setSpecInfoToList(table)
  table.forEach(v => {
    // 钢卷按米显示
    if (v.basicClass === matClsEnum.STEEL_COIL.V) {
      v.measureUnit = '米'
      v.measurePrecision = 3
    }
  })
  await numFmtByBasicClass(table)
  // 项目可能是多个，显示在表尾
  const projectName = header?.project ? `${header?.project?.serialNumber} ${header?.project?.shortName}` : '-'
  return {
    header,
    table,
    qrCode,
    footer: { projectName, remark: header.remark }
  }
}

export default {
  handleOrderName,
  handleRequisitionsRecord
}
