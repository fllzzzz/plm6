import { baseMaterialTypeEnum, purchaseOrderPaymentModeEnum, purchaseStatusEnum, orderSupplyTypeEnum } from '@enum-ms/wms'
import { invoiceTypeEnum, settlementStatusEnum, weightMeasurementModeEnum } from '@enum-ms/finance'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'

// 获取采购合同
const getPurchaseOrder = {
  url: '/api/scm/purchase-order',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        content: [
          {
            id: 1, // 订单id
            purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
            supplyType: orderSupplyTypeEnum.PARTY_A.V, // 供应类型
            basicClass: 7, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            // pickUpMode: pickUpModeEnum.SUPPLIER.V, // 提货方式
            requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
            preparationSNList: ['SP-PSN-QER-IOP-KOL-1', 'SP-PSN-QER-IOP-KOL-2'], // 备料单
            supplier: {
              id: 5,
              name: '杭州天马钢材有限公司'
            },
            branchCompany: {
              id: 2,
              name: '杭州初鸣建筑科技有限公司'
            },
            'mete|1000-10000.1-2': 1000, // 合同量量
            'amount|100000-1000000.1-2': 100000, // 合同金额
            meteUnit: '千克', // 单位
            'taxRate|1-4': 3, // 税率（百分比）
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流运输方式
            weightMeasurementMode: weightMeasurementModeEnum.MIXTURE.V, // 重量计量方式
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            remark: '@cparagraph', // 备注
            attachments: [{ id: 1, name: '钢板清单.png', createTime: 1635470149881 }], // 附件
            applicantName: '@cname', // 创建人
            lastOperatorName: '@cname', // 最后编辑人
            purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
            settlementStatus: settlementStatusEnum.UNSETTLEMENT.V, // 结算状态（订单是否结算，结算时，自动将采购状态设置为完成，且无法再发开采购状态）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 5, // 订单id
            purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            auxMaterialIds: [196, 197, 198, 199],
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
            preparationSNList: ['SP-PSN-QER-IOP-KOL-3', 'SP-PSN-QER-IOP-KOL-4'], // 备料单
            branchCompany: {
              id: 2,
              name: '杭州初鸣建筑科技有限公司'
            },
            supplier: {
              id: 5,
              name: '杭州天马钢材有限公司'
            },
            'mete|1000-10000.1-2': 1000, // 合同量量
            'amount|100000-1000000.1-2': 100000, // 合同金额
            meteUnit: '千克', // 单位
            'taxRate|1-4': 3, // 税率（百分比）
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流运输方式
            weightMeasurementMode: weightMeasurementModeEnum.MIXTURE.V, // 重量计量方式
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            remark: '@cparagraph', // 备注
            attachments: [{ id: 1, name: '钢板清单.png', createTime: 1635470149881 }], // 附件
            applicantName: '@cname', // 创建人
            lastOperatorName: '@cname', // 最后编辑人
            purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
            settlementStatus: settlementStatusEnum.UNSETTLEMENT.V, // 结算状态（订单是否结算，结算时，自动将采购状态设置为完成，且无法再发开采购状态）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 10, // 订单id
            purchaseType: baseMaterialTypeEnum.MANUFACTURED.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: 32, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            strucAreaIds: [1, 5], // 构件区域id
            enclAreaIds: [2], // 围护区域id
            requisitionsSN: ['AFTER-Q-123456', 'AFTER-Q-133456'], // 采购申请单
            branchCompany: {
              id: 1,
              name: '杭州初鸣建筑科技有限公司'
            },
            supplier: {
              id: 1,
              name: '杭州天马钢材有限公司'
            },
            'mete|1000-10000.1-2': 1000, // 合同量量
            'amount|100000-1000000.1-2': 100000, // 合同金额
            meteUnit: '千克', // 单位
            'taxRate|1-4': 3, // 税率（百分比）
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.POST.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.SUPPLIER.V, // 物流运输方式
            weightMeasurementMode: weightMeasurementModeEnum.THEORY.V, // 重量计量方式
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            remark: '@cparagraph', // 备注
            attachments: [{ id: 1, name: '钢板清单.png', createTime: 1635470149881 }], // 附件
            applicantName: '@cname', // 创建人
            lastOperatorName: '@cname', // 最后编辑人
            purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
            settlementStatus: settlementStatusEnum.SETTLED.V, // 结算状态（订单是否结算，结算时，自动将采购状态设置为完成，且无法再发开采购状态）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            boolUsed: true // 是否使用(使用状态下部分字段无法修改)
          }
        ],
        totalElements: 2
      },
      message: '操作成功'
    }
  }
}

// 获取采购中的订单（简要）
const getPurchasingPurchaseOrderBrief = {
  url: '/api/scm/purchase-order/purchasing/all/brief',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        content: [
          {
            id: 1, // 订单id
            purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: 7, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            // pickUpMode: pickUpModeEnum.SUPPLIER.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.SUPPLIER.V, // 物流费用承担方
            requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
            supplier: {
              id: 1,
              name: '杭州天马钢材有限公司'
            }
          },
          {
            id: 2, // 订单id
            purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: 6, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            // pickUpMode: pickUpModeEnum.SUPPLIER.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流运输方式
            requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
            supplier: {
              id: 1,
              name: '杭州天马钢材有限公司'
            }
          },
          {
            id: 3, // 订单id
            purchaseType: baseMaterialTypeEnum.MANUFACTURED.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            auxMaterialIds: [196, 197, 198, 199],
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.POST.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流费用承担方
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
            strucAreaIds: [1, 5], // 构件区域id
            enclAreaIds: [2], // 围护区域id
            requisitionsSN: ['AFTER-Q-123456', 'AFTER-Q-133456'], // 采购申请单
            supplier: {
              // 供应商
              id: 1,
              name: '杭州天天向上有限公司'
            }
          },
          {
            id: 5, // 订单id
            purchaseType: baseMaterialTypeEnum.MANUFACTURED.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            auxMaterialIds: [196, 197],
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流费用承担方
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
            strucAreaIds: [1, 5], // 构件区域id
            enclAreaIds: [2], // 围护区域id
            requisitionsSN: ['AFTER-Q-123456', 'AFTER-Q-133456'], // 采购申请单
            supplier: {
              // 供应商
              id: 1,
              name: '杭州天天向上有限公司'
            }
          },
          {
            id: 4, // 订单id
            purchaseType: baseMaterialTypeEnum.MANUFACTURED.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: matClsEnum.GAS.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
            logisticsPayerType: logisticsPayerEnum.SUPPLIER.V, // 物流费用承担方
            purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
            strucAreaIds: [1, 5], // 构件区域id
            enclAreaIds: [2], // 围护区域id
            requisitionsSN: ['AFTER-Q-123456'], // 采购申请单
            supplier: {
              // 供应商
              id: 1,
              name: '杭州决明子有限公司'
            }
          }
        ],
        totalElements: 5
      },
      message: '操作成功'
    }
  }
}

// 详情
const detail_124 = {
  url: RegExp('/api/scm/purchase-order/' + '[124]'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 1, // 订单id
        boolUsed: true, // 是否已使用
        purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
        supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
        basicClass: matClsEnum.STEEL_PLATE.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
        'projects|2': [
          {
            'id|+1': 1,
            'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
            'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          }
        ], // 项目id
        requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
        branchCompany: {
          id: 1,
          name: '杭州初鸣建筑科技有限公司'
        },
        supplier: {
          // 供应商
          id: 1,
          name: '天马钢材有限公司'
        },
        'mete|1000-10000.1-2': 1000, // 合同量量
        'amount|100000-1000000.1-2': 100000, // 合同金额
        meteUnit: '千克', // 单位
        'taxRate|1-4': 3, // 税率（百分比）
        invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
        // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
        logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
        logisticsPayerType: logisticsPayerEnum.SUPPLIER.V, // 物流费用承担方
        weightMeasurementMode: weightMeasurementModeEnum.MIXTURE.V, // 重量计量方式
        purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
        remark: '@cparagraph', // 备注
        attachments: [{ id: 1, name: '钢板清单.png', createTime: 1635470149881 }], // 附件
        applicantName: '@cname', // 创建人
        lastOperatorName: '@cname', // 最后编辑人
        purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
        settlementStatus: settlementStatusEnum.UNSETTLEMENT.V, // 结算状态（订单是否结算，结算时，自动将采购状态设置为完成，且无法再发开采购状态）
        createTime: '@datetime(T)', // 创建时间
        updateTime: '@datetime(T)', // 修改时间
        userUpdateTime: '@datetime(T)', // 用户修改时间
        preparationSNList: ['SP-PSN-QER-IOP-KOL-1', 'SP-PSN-QER-IOP-KOL-2'], // 备料单
        preparationList: [
          {
            id: 1,
            preparationSN: 'SP-PSN-QER-IOP-KOL-1',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000, // 核算量
            quantity: 10, // 数量
            thickness: 10,
            length: 11000,
            width: 990,
            brand: '鞍钢'
          },
          {
            id: 2,
            preparationSN: 'SP-PSN-QER-IOP-KOL-1',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000, // 核算量
            quantity: 10, // 数量
            thickness: 10,
            length: 11000,
            width: 990,
            brand: '马钢'
          },
          {
            id: 3,
            preparationSN: 'SP-PSN-QER-IOP-KOL-2',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000, // 核算量
            quantity: 10, // 数量
            thickness: 10,
            length: 11000,
            width: 990,
            brand: '马钢'
          }
        ]
      }
    }
  }
}

// 详情
const detail_35 = {
  url: RegExp('/api/scm/purchase-order/' + '[35]'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 1, // 订单id
        purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
        supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
        basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
        auxMaterialIds: [196, 197, 198, 199],
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
        'projects|2': [
          {
            'id|+1': 1,
            'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
            'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          }
        ], // 项目id
        requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
        'preparationSNList|2-5': [/([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/], // 备料单
        branchCompany: {
          id: 1,
          name: '杭州初鸣建筑科技有限公司'
        },
        supplier: {
          // 供应商
          id: 1,
          name: '天马钢材有限公司'
        },
        'mete|1000-10000.1-2': 1000, // 合同量量
        'amount|100000-1000000.1-2': 100000, // 合同金额
        meteUnit: '千克', // 单位
        'taxRate|1-4': 3, // 税率（百分比）
        invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
        // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
        logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
        logisticsPayerType: logisticsPayerEnum.SUPPLIER.V, // 物流费用承担方
        weightMeasurementMode: weightMeasurementModeEnum.MIXTURE.V, // 重量计量方式
        purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
        remark: '@cparagraph', // 备注
        attachments: [{ id: 1, name: '钢板清单.png', createTime: 1635470149881 }], // 附件
        applicantName: '@cname', // 创建人
        lastOperatorName: '@cname', // 最后编辑人
        purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
        settlementStatus: settlementStatusEnum.UNSETTLEMENT.V, // 结算状态（订单是否结算，结算时，自动将采购状态设置为完成，且无法再发开采购状态）
        createTime: '@datetime(T)', // 创建时间
        updateTime: '@datetime(T)', // 修改时间
        userUpdateTime: '@datetime(T)', // 用户修改时间
        preparationSNList: ['SP-PSN-QER-IOP-KOL-3', 'SP-PSN-QER-IOP-KOL-4'], // 备料单
        preparationList: [
          {
            id: 1,
            preparationSN: 'SP-PSN-QER-IOP-KOL-3',
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            brand: '嘻嘻',
            mete: 10
          },
          {
            id: 2,
            preparationSN: 'SP-PSN-QER-IOP-KOL-3',
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            brand: '哈哈',
            mete: 10
          },
          {
            id: 3,
            preparationSN: 'SP-PSN-QER-IOP-KOL-3',
            classifyId: 247,
            color: '天蓝色',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            brand: '马克',
            mete: 25000
          },
          {
            id: 4,
            preparationSN: 'SP-PSN-QER-IOP-KOL-4',
            classifyId: 247,
            color: '天蓝色',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            brand: '马克',
            mete: 25000
          }
        ]
      }
    }
  }
}

// 添加采购合同
const add = {
  url: '/api/scm/purchase-order',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改采购合同
const edit = {
  url: '/api/scm/purchase-order',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改采购合同状态
const editPurchaseStatus = {
  url: '/api/scm/purchase-order/purchase-status',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除采购合同
const del = {
  url: '/api/scm/purchase-order',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 下载excel表格
const download = {
  url: '/api/scm/purchase-order/export',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功'
    }
  }
}

export default [getPurchasingPurchaseOrderBrief, getPurchaseOrder, detail_124, detail_35, add, edit, editPurchaseStatus, del, download]
