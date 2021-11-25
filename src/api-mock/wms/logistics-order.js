import { validatorLicensePlate } from '@/utils/validate/pattern'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { baseMaterialTypeEnum } from '@enum-ms/wms'

// 获取采购订单
const getList = {
  url: '/api/wms/logistics-order',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        content: [
          {
            id: 1, // 订单id
            'basicClass|1-16': 1, // 采购物料基础类型
            purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            plateNumber: validatorLicensePlate, // 车牌号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            loadingWeight: 1000.00, // 装载重量（kg）TODO:入库过磅重量较好?
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            freight: 1200, // 运费
            priceExcludingVAT: 1165.05, // 不含税金额
            inputVAT: 34.95, // 进项税
            inboundSN: '21/11/25/R-001', // 入库单号
            remark: '@cparagraph', // 备注
            supplierId: 1, // 供应商id
            founderName: '@cname', // 创建人
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userEditTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 订单id
            'basicClass|1-16': 1, // 采购物料基础类型
            purchaseType: baseMaterialTypeEnum.MANUFACTURED.V, // 采购类型
            plateNumber: validatorLicensePlate, // 车牌号
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            loadingWeight: undefined, // 装载重量（kg）
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            freight: 2600, // 运费
            priceExcludingVAT: 2524.37, // 不含税金额
            inputVAT: 75.63, // 进项税
            inboundSN: '21/11/25/R-002', // 入库单号
            remark: '@cparagraph', // 备注
            supplierId: 2, // 供应商id
            founderName: '@cname', // 创建人
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userEditTime: '@datetime(T)' // 用户修改时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

export default [
  getList
]
