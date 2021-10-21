import { inboundFillWayEnum } from '@enum-ms/wms'

// 获取入库基础配置
const getInboundBasicConf = {
  url: '/api/config/v1/wms/inbound/base',
  method: 'get',
  timeout: 2000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'amountFillWay': inboundFillWayEnum.AUDITING.V,
        'factoryFillWay': inboundFillWayEnum.APPLICATION.V
      }
    }
  }
}

// 设置入库基础配置
const setInboundBasicConf = {
  url: '/api/config/v1/wms/inbound/base',
  method: 'put',
  timeout: 2000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getInboundBasicConf,
  setInboundBasicConf
]
