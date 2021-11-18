import { purchaseTypeEnum, pickUpModeEnum, purchaseOrderPaymentModeEnum, purchaseStatusEnum, orderSupplyTypeEnum } from '@enum-ms/wms'
import { invoiceTypeEnum, settlementStatusEnum, weightMeasurementModeEnum } from '@enum-ms/finance'

// 获取采购订单
const getPurchaseOrder = {
  url: '/api/wms/purchase-order',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        content: [
          {
            id: 1, // 订单id
            purchaseType: purchaseTypeEnum.RAW_MATERIAL.V, // 采购类型
            supplyType: orderSupplyTypeEnum.PARTY_A.V, // 供应类型
            'basicClass|1-16': 1, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'project|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            requisitionsSN: ['SG-2021111801', 'SG-2021111823'], // 采购申请单
            supplierId: 1, // 供应商id
            'mete|1000-10000.1-2': 1000, // 合同量量
            'amount|100000-1000000.1-2': 100000, // 合同金额
            meteUnit: '千克', // 单位
            'taxRate|1-4': 3, // 税率（百分比）
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            weightMeasurementMode: weightMeasurementModeEnum.THEORY.V, // 重量计量方式
            paymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            remark: '', // 备注
            attachments: [{ id: 1, name: '钢板清单.png', createTime: 1635470149881 }], // 附件
            founderName: '@cname', // 创建人
            lastOperatorName: '@cname', // 最后编辑人
            purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
            settlementStatus: settlementStatusEnum.UNSETTLEMENT.V, // 结算状态（订单是否结算，结算时，自动将采购状态设置为完成，且无法再发开采购状态）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)' // 修改时间
          },
          {
            id: 1, // 订单id
            purchaseType: purchaseTypeEnum.MANUFACTURED.V, // 采购类型
            supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
            basicClass: 32, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'project|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            requisitionsSN: ['SG-2021111801', 'SG-2021111823'], // 采购申请单
            supplierId: 1, // 供应商id
            'mete|1000-10000.1-2': 1000, // 合同量量
            'amount|100000-1000000.1-2': 100000, // 合同金额
            meteUnit: '千克', // 单位
            'taxRate|1-4': 3, // 税率（百分比）
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
            weightMeasurementMode: weightMeasurementModeEnum.THEORY.V, // 重量计量方式
            paymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
            remark: '', // 备注
            attachments: [{ id: 1, name: '钢板清单.png', createTime: 1635470149881 }], // 附件
            founderName: '@cname', // 创建人
            lastOperatorName: '@cname', // 最后编辑人
            purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
            settlementStatus: settlementStatusEnum.UNSETTLEMENT.V, // 结算状态（订单是否结算，结算时，自动将采购状态设置为完成，且无法再发开采购状态）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)' // 修改时间
          }
        ],
        totalElements: 2
      },
      message: '操作成功'
    }
  }
}

// 修改采购单状态状态
const editPurchaseStatus = {
  url: '/api/wms/purchase-order/purchase-status',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除单位
const del = {
  url: '/api/wms/purchase-order',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [getPurchaseOrder, editPurchaseStatus, del]
