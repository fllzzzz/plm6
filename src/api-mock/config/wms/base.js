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
            overDiffSubmittable: whetherEnum.TRUE.V
          },
          fillWay: {
            amountFillWay: inboundFillWayEnum.REVIEWING.V, // 金额填写场景
            warehouseFillWay: inboundFillWayEnum.REVIEWING.V // 存储位置填写场景
          },
          printLabelTipWay: { // 打印标签提示场景
            afterApplication: true, // 入库申请提交后
            afterReview: true // 审核后
          }
        },
        outbound: {
          // 辅材出库方式
          boolAuxMatToWorkShopWay: whetherEnum.FALSE.V,
          // 气体入库直接出库
          boolGasOutAfterInbound: whetherEnum.TRUE.V,
          // 项目仓可以出库给其他项目
          boolCanOutToOtherProject: whetherEnum.TRUE.V
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
        'amountFillWay': inboundFillWayEnum.REVIEWING.V, // 金额填写场景
        'warehouseFillWay': inboundFillWayEnum.APPLICATION.V, // 存储位置填写场景
        'printLabelTipWay': { // 打印标签提示场景
          afterApplication: true,
          afterReview: true
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
        // 辅材出库到车间的配置
        boolAuxMatToWorkShopWay: whetherEnum.FALSE.V,
        // 气体入库直接出库
        boolGasOutAfterInbound: whetherEnum.TRUE.V,
        // 项目仓可以出库给其他项目
        boolCanOutToOtherProject: whetherEnum.TRUE.V
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

// 获取甲供材料归还基础配置
const getPartyABorrowReturnConf = {
  url: '/api/wms/config/transfer/party-a-borrow-return',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        boolReturnByOtherProject: true, // 可从非借用项目归还
        steelPlateSideLengthDiff: 0, // 钢板-长宽长度误差(mm)
        sectionSteelLengthDiff: 0, // 型钢-长度误差(mm)
        steelCoilLengthDiff: 0 // 钢卷-长度误差(mm)
      }
    }
  }
}

// 保存甲供材料归还基础配置
const setPartyABorrowReturnConf = {
  url: '/api/wms/config/transfer/party-a-borrow-return',
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
  setOutboundBasicConf,
  getPartyABorrowReturnConf,
  setPartyABorrowReturnConf
]
