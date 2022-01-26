// 钢材退库申请
const steelPlateReturnApplication = {
  url: '/api/wms/return/application/steel-plate',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 型材退库申请
const sectionSteelReturnApplication = {
  url: '/api/wms/return/application/section-steel',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 钢卷退库申请
const steelCoilReturnApplication = {
  url: '/api/wms/return/application/steel-coil',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 辅材退库申请
const auxMatReturnApplication = {
  url: '/api/wms/return/application/auxiliary-material',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 气体退库申请
const gasReturnApplication = {
  url: '/api/wms/return/application/gas',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}
export default [
  steelPlateReturnApplication,
  sectionSteelReturnApplication,
  steelCoilReturnApplication,
  auxMatReturnApplication,
  gasReturnApplication
]
