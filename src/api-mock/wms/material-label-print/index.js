// 添加采购订单
const addPrintRecord = {
  url: '/api/wms/material/label-print/print-record',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 添加采购订单
const getNotPrintedMaterialNumber = {
  url: '/api/wms/material/label-print/not-printed-number',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: { 'inboundMaterial|1-10': 10, 'outboundMaterial|1-3': 0, 'transferMaterial|1-5': 5, 'returnMaterial|1-2': 2 }
    }
  }
}

export default [addPrintRecord, getNotPrintedMaterialNumber]
