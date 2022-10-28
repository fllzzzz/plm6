import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

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
  await numFmtByBasicClass(table)
  // 项目可能是多个，显示在表尾
  const projectName = (header?.project || []).map(v => `${v.serialNumber} ${v.shortName}`)?.join('、')
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
