// 钢板出库办理
const steelPlateOutboundHandling = {
  url: '/api/wms/outbound/application/steel-plate',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 型材出库办理
const sectionSteelOutboundHandling = {
  url: '/api/wms/outbound/application/section-steel',
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
const steelCoilOutboundHandling = {
  url: '/api/wms/outbound/application/steel-coil',
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
const auxMatOutboundHandling = {
  url: '/api/wms/outbound/application/auxiliary-material',
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
const gasOutboundHandling = {
  url: '/api/wms/outbound/application/gas',
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
const steelPlateBatchOutboundHandling = {
  url: '/api/wms/outbound/application/steel-plate/batch',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 型材批量出库办理
const sectionSteelBatchOutboundHandling = {
  url: '/api/wms/outbound/application/section-steel/batch',
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
const steelCoilBatchOutboundHandling = {
  url: '/api/wms/outbound/application/steel-coil/batch',
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
const auxMatBatchOutboundHandling = {
  url: '/api/wms/outbound/application/auxiliary-material/batch',
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
const gasBatchOutboundHandling = {
  url: '/api/wms/outbound/application/gas/batch',
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
  steelPlateOutboundHandling,
  sectionSteelOutboundHandling,
  steelCoilOutboundHandling,
  auxMatOutboundHandling,
  gasOutboundHandling,
  steelPlateBatchOutboundHandling,
  sectionSteelBatchOutboundHandling,
  steelCoilBatchOutboundHandling,
  auxMatBatchOutboundHandling,
  gasBatchOutboundHandling
]
