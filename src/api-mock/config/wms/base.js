import { inboundFillWayEnum } from '@enum-ms/wms'
import { numOrPctEnum, whetherEnum } from '@enum-ms/common'

// 获取入库基础配置
const getInboundBasicConf = {
  url: '/api/wms/config/inbound/base',
  method: 'get',
  timeout: 1000,
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
  url: '/api/wms/config/inbound/base',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取入库钢材配置
const getInboundSteelConf = {
  url: '/api/wms/config/inbound/steel',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        // 车次重量差值
        trainsDiff: 0,
        // 车次重量差值类型（g 或 %）
        trainsDiffType: numOrPctEnum.NUMBER.V,
        // 单件钢材差值
        steelDiff: 0,
        // 差值类型（g 或 %）
        steelDiffType: numOrPctEnum.PERCENTAGE.V
      }
    }
  }
}

// 保存入库钢材配置
const setInboundSteelConf = {
  url: '/api/wms/config/inbound/steel',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取出库基础配置
const getOutboundBasicConf = {
  url: '/api/wms/config/outbound/base',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'boolAuxMatToWorkShopWay': whetherEnum.FALSE.V
      }
    }
  }
}

// 设置出库基础配置
const setOutboundBasicConf = {
  url: '/api/wms/config/outbound/base',
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
  getInboundBasicConf,
  setInboundBasicConf,
  getInboundSteelConf,
  setInboundSteelConf,
  getOutboundBasicConf,
  setOutboundBasicConf
]
