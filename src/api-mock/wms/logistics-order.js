import { logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { patternLicensePlate } from '@/utils/validate/pattern'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { baseMaterialTypeEnum } from '@enum-ms/wms'

// 获取采购订单（入库审核通过的）
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
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 系统物流编号
            inboundSN: '21/11/25/R-001', // 入库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            licensePlate: patternLicensePlate, // 车牌号
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V,
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            loadingWeight: 666666, // 装载重量（g）TODO:入库过磅重量较好?
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            freight: 1200, // 运费
            amountExcludingVAT: 1165.05, // 不含税金额
            inputVAT: 34.95, // 进项税
            remark: '@cparagraph', // 备注
            supplierId: 1, // 供应商id
            applicantName: '@cname', // 创建人（填写物流信息的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 订单id
            'basicClass|1-16': 1, // 采购物料基础类型
            purchaseType: baseMaterialTypeEnum.MANUFACTURED.V, // 采购类型
            licensePlate: patternLicensePlate, // 车牌号
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
            inboundSN: '21/11/25/R-002', // 入库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            logisticsTransportType: logisticsTransportTypeEnum.POST.V,
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
            amountExcludingVAT: 2524.37, // 不含税金额
            inputVAT: 75.63, // 进项税
            remark: '@cparagraph', // 备注
            supplierId: 2, // 供应商id
            applicantName: '@cname', // 创建人
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

export default [getList]
