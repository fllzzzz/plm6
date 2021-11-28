import { inboundFillWayEnum } from '@enum-ms/wms'
import { numOrPctEnum, whetherEnum } from '@enum-ms/common'

const getWmsConfig = {
  url: '/api/wms/config/all',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        inbound: {
          steel: { // 钢材配置
            // 车次重量差值
            trainsDiff: 1000,
            // 车次重量差值类型（g 或 %）
            trainsDiffType: numOrPctEnum.NUMBER.V,
            // 单件钢材差值
            steelDiff: 100,
            // 差值类型（g 或 %）
            steelDiffType: numOrPctEnum.NUMBER.V,
            // 误差超过范围可提交
            overDiffSubmittable: whetherEnum.FALSE.V
          },
          fillWay: {
            amountFillWay: inboundFillWayEnum.AUDITING.V, // 金额填写场景
            factoryFillWay: inboundFillWayEnum.APPLICATION.V // 工厂填写场景
          },
          printLabelTipWay: { // 打印标签提示场景
            afterApplication: true, // 入库申请提交后
            afterAudit: true // 审核后
          }
        },
        outbound: {
          // 辅材出库方式
          boolAuxMatToWorkShopWay: whetherEnum.FALSE.V
        }
      }
    }
  }
}

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
        'amountFillWay': inboundFillWayEnum.AUDITING.V, // 金额填写场景
        'factoryFillWay': inboundFillWayEnum.APPLICATION.V, // 工厂填写场景
        'printLabelTipWay': { // 打印标签提示场景
          afterApplication: true,
          afterAudit: true
        }
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
        steelDiffType: numOrPctEnum.PERCENTAGE.V,
        // 误差超过范围可提交(默认可提交)
        overDiffSubmittable: whetherEnum.TRUE.V
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
  getWmsConfig,
  getInboundBasicConf,
  setInboundBasicConf,
  getInboundSteelConf,
  setInboundSteelConf,
  getOutboundBasicConf,
  setOutboundBasicConf
]
