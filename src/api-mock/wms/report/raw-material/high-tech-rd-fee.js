import { matClsEnum } from '@/utils/enum/modules/classification'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { materialIsWholeEnum, materialOutboundModeEnum } from '@/utils/enum/modules/wms'

// 出库明细
const get = {
  url: '/api/wms/report/raw-materials/high-tech-rd-fee',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 2,
            materialIsWhole: materialIsWholeEnum.ODDMENT.V, // 物料类型（整料|余料）
            materialOutboundMode: materialOutboundModeEnum.WHOLE.V, // 物料出库方式
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specification: 'Q235B',
            quantity: 5,
            thickness: 20,
            length: 1500,
            width: 2000,
            brand: '哈哈',
            heatNoAndBatchNo: 'fddfd',
            mete: 2355000,
            weight: 2355000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.02, // 含税单价
            unitPriceExcludingVAT: 0.017, // 不含税单价
            amount: 47100, // 含税金额
            amountExcludingVAT: 41681.42, // 不含税金额
            inputVAT: 5418.58,
            'rdRate|1-5.2': 1,
            'rdFee|100-5000.2': 1,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundReceipt: {
              id: 1, // 出库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
              applicantName: '@cname', // 创建人（填写出库的人）
              reviewerName: '@cname', // 审核人（审核的人）
              outboundTime: '@datetime(T)', // 出库时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 3,
            specification: '57*21*3*9 * Q325B',
            nationalStandard: 'GB-10', // 国家标准
            classifyId: 110,
            materialIsWhole: materialIsWholeEnum.ODDMENT.V, // 物料类型（整料|余料）
            materialOutboundMode: materialOutboundModeEnum.HALF.V, // 物料出库方式
            basicClass: matClsEnum.SECTION_STEEL.V,
            quantity: 2,
            length: 10000,
            brand: '马钢',
            heatNoAndBatchNo: 'ooopp',
            mete: 252900,
            weight: 252900,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.03,
            unitPriceExcludingVAT: 0.017, // 不含税单价
            amount: 9174,
            amountExcludingVAT: 8188.58,
            inputVAT: 1055.42,
            'rdRate|1-5.2': 1,
            'rdFee|100-5000.2': 1,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundReceipt: {
              id: 1, // 出库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
              applicantName: '@cname', // 创建人（填写出库的人）
              reviewerName: '@cname', // 审核人（审核的人）
              outboundTime: '@datetime(T)', // 出库时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 200000,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: '12341234fsafs1234',
            thickness: 0.326,
            length: 3907.62,
            width: 1000,
            mete: 200000,
            weight: 1000000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.05,
            unitPriceExcludingVAT: 0.045, // 不含税单价
            amount: 500,
            amountExcludingVAT: 450,
            inputVAT: 50,
            'rdRate|1-5.2': 1,
            'rdFee|100-5000.2': 1,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 4,
              name: '668号仓库'
            },
            outboundReceipt: {
              id: 1, // 出库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
              applicantName: '@cname', // 创建人（填写出库的人）
              reviewerName: '@cname', // 审核人（审核的人）
              outboundTime: '@datetime(T)', // 出库时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 6,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            mete: 80,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            unitPriceExcludingVAT: 0.009, // 不含税单价
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            'rdRate|1-5.2': 1,
            'rdFee|100-5000.2': 1,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundReceipt: {
              id: 1, // 出库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
              applicantName: '@cname', // 创建人（填写出库的人）
              reviewerName: '@cname', // 审核人（审核的人）
              outboundTime: '@datetime(T)', // 出库时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 7,
            classifyId: 247,
            basicClass: matClsEnum.STEEL_PLATE.V,
            quantity: 10,
            brand: '嘻嘻',
            color: '蓝色',
            mete: 100000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            unitPriceExcludingVAT: 0.009, // 不含税单价
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            'rdRate|1-5.2': 1,
            'rdFee|100-5000.2': 1,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundReceipt: {
              id: 1, // 出库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
              applicantName: '@cname', // 创建人（填写出库的人）
              reviewerName: '@cname', // 审核人（审核的人）
              outboundTime: '@datetime(T)', // 出库时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 8,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            mete: 200000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            unitPriceExcludingVAT: 0.009, // 不含税单价
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            'rdRate|1-5.2': 1,
            'rdFee|100-5000.2': 1,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundReceipt: {
              id: 1, // 出库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
              applicantName: '@cname', // 创建人（填写出库的人）
              reviewerName: '@cname', // 审核人（审核的人）
              outboundTime: '@datetime(T)', // 出库时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          }
        ],
        totalElements: 6,
        'rdFeeForMonth|10000-60000.2': 1,
        'rdFeeForYear|100000-600000.2': 1
      }
    }
  }
}

export default [get]
