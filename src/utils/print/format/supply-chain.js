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

export default {
  handleOrderName
}
