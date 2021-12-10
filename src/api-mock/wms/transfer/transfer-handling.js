// 钢板出库办理
const steelPlateTransferHandling = {
  url: '/api/wms/material-transfer/steel-plate',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 型钢出库办理
const sectionSteelTransferHandling = {
  url: '/api/wms/material-transfer/section-steel',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 钢卷出库办理
const steelCoilTransferHandling = {
  url: '/api/wms/material-transfer/steel-coil',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 辅材出库办理
const auxMatTransferHandling = {
  url: '/api/wms/material-transfer/auxiliary-material',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 气体出库办理
const gasTransferHandling = {
  url: '/api/wms/material-transfer/gas',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 钢板批量出库办理
const steelPlateBatchTransferHandling = {
  url: '/api/wms/material-transfer/steel-plate/batch',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 型钢批量出库办理
const sectionSteelBatchTransferHandling = {
  url: '/api/wms/material-transfer/section-steel/batch',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 钢卷批量出库办理
const steelCoilBatchTransferHandling = {
  url: '/api/wms/material-transfer/steel-coil/batch',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 辅材批量出库办理
const auxMatBatchTransferHandling = {
  url: '/api/wms/material-transfer/auxiliary-material/batch',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 气体批量出库办理
const gasBatchTransferHandling = {
  url: '/api/wms/material-transfer/gas/batch',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  steelPlateTransferHandling,
  sectionSteelTransferHandling,
  steelCoilTransferHandling,
  auxMatTransferHandling,
  gasTransferHandling,
  steelPlateBatchTransferHandling,
  sectionSteelBatchTransferHandling,
  steelCoilBatchTransferHandling,
  auxMatBatchTransferHandling,
  gasBatchTransferHandling
]
