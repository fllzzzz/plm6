import { matClsEnum } from '@/utils/enum/modules/classification'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { measureTypeEnum } from '@/utils/enum/modules/wms'

// 收发存报表
const getSendAndReceiveStorage = {
  url: '/api/wms/report/raw-materials/send-and-receive-storage',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        totalAmount: {
          // 总额
          beginPeriod: 10000000, // 期初总额（当月）
          endPeriod: 9000000, // 期末总额（当月）
          inbound: 2000000, // 入库总额（当月）
          outbound: 3000000 // 出库总额（当月）
        },
        content: [
          {
            id: 1, // 记录id
            unitType: measureTypeEnum.ACCOUNTING.V,
            boolPartyA: true,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specifications: ['Q325B'],
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
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
            beginPeriod: {
              mete: 800000,
              quantity: 10,
              unitPrice: 0.05,
              unitPriceExcludingVAT: 0.05,
              amount: 4000,
              amountExcludingVAT: 4000,
              inputVAT: 50
            },
            endPeriod: {
              mete: 400000,
              quantity: 5,
              unitPrice: 0.05,
              unitPriceExcludingVAT: 0.05,
              amount: 2000,
              amountExcludingVAT: 2000,
              inputVAT: 25
            },
            inbound: {
              mete: 80000,
              quantity: 1,
              unitPrice: 0.05,
              unitPriceExcludingVAT: 0.05,
              amount: 400,
              amountExcludingVAT: 400,
              inputVAT: 5
            },
            outbound: {
              mete: 480000,
              quantity: 6,
              unitPrice: 0.05,
              unitPriceExcludingVAT: 0.05,
              amount: 2400,
              amountExcludingVAT: 2400,
              inputVAT: 30
            }
          },
          {
            id: 2,
            unitType: measureTypeEnum.ACCOUNTING.V,
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specifications: ['Q325B'],
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
            unitPrice: 0.02,
            unitPriceExcludingVAT: 0.02,
            amount: 47100,
            amountExcludingVAT: 41681.42,
            inputVAT: 5418.58,
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
            beginPeriod: {
              mete: 1000000,
              quantity: 10,
              unitPrice: 0.06,
              unitPriceExcludingVAT: 0.06,
              amount: 6000,
              amountExcludingVAT: 6000,
              inputVAT: 110
            },
            endPeriod: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            },
            inbound: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            },
            outbound: {
              mete: 1000000,
              quantity: 10,
              unitPrice: 0.06,
              unitPriceExcludingVAT: 0.06,
              amount: 6000,
              amountExcludingVAT: 6000,
              inputVAT: 110
            }
          },
          {
            id: 3,
            boolOddmentByHalfOut: true, // 是否为整料半出剩下的余料
            unitType: measureTypeEnum.ACCOUNTING.V,
            specifications: ['57*21*3*9 * Q325B'],
            classifyId: 110,
            basicClass: matClsEnum.SECTION_STEEL.V,
            length: 10000,
            brand: '马钢',
            heatNoAndBatchNo: 'ooopp',
            weight: 252900,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
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
            beginPeriod: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            },
            endPeriod: {
              mete: 1000000,
              quantity: 10,
              unitPrice: 0.06,
              unitPriceExcludingVAT: 0.06,
              amount: 6000,
              amountExcludingVAT: 6000,
              inputVAT: 110
            },
            inbound: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            },
            outbound: {
              mete: -1000000,
              quantity: -10,
              unitPrice: -0.06,
              unitPriceExcludingVAT: -0.06,
              amount: -6000,
              amountExcludingVAT: -6000,
              inputVAT: -110
            }
          },
          {
            id: 5,
            unitType: measureTypeEnum.ACCOUNTING.V,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specifications: ['DC51D+Z'],
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
            unitPriceExcludingVAT: 0.05,
            amount: 500,
            amountExcludingVAT: 450,
            inputVAT: 50,
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
            beginPeriod: {
              mete: 10000000,
              quantity: 1000000,
              unitPrice: 0.01,
              unitPriceExcludingVAT: 0.01,
              amount: 100000,
              amountExcludingVAT: 100000,
              inputVAT: 1100
            },
            endPeriod: {
              mete: 9000000,
              quantity: 900000,
              unitPrice: 0.01,
              unitPriceExcludingVAT: 0.01,
              amount: 90000,
              amountExcludingVAT: 90000,
              inputVAT: null
            },
            inbound: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: 990
            },
            outbound: {
              mete: 1000000,
              quantity: 100000,
              unitPrice: 0.01,
              unitPriceExcludingVAT: 0.01,
              amount: 10000,
              amountExcludingVAT: 10000,
              inputVAT: 110
            }
          },
          {
            id: 6,
            unitType: measureTypeEnum.ACCOUNTING.V,
            classifyId: 204,
            specifications: ['M27 * 60', 'M27 * 65', 'M26 * 60'],
            basicClass: matClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            mete: 80,
            unitPrice: 0.01,
            unitPriceExcludingVAT: 0.01,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
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
            beginPeriod: {
              mete: 500,
              quantity: 500,
              unitPrice: 1,
              unitPriceExcludingVAT: 1,
              amount: 500,
              amountExcludingVAT: 500,
              inputVAT: 5
            },
            endPeriod: {
              mete: 500,
              quantity: 500,
              unitPrice: 1,
              unitPriceExcludingVAT: 1,
              amount: 500,
              amountExcludingVAT: 500,
              inputVAT: 5
            },
            inbound: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: 990
            },
            outbound: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            }
          },
          {
            id: 7,
            unitType: measureTypeEnum.ACCOUNTING.V,
            classifyId: 247,
            basicClass: matClsEnum.STEEL_PLATE.V,
            quantity: 10,
            brand: '嘻嘻',
            color: '蓝色',
            mete: 100000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            unitPriceExcludingVAT: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
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
            beginPeriod: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            },
            endPeriod: {
              mete: 1000000,
              quantity: 50,
              unitPrice: 1,
              unitPriceExcludingVAT: 1,
              amount: 1000,
              amountExcludingVAT: 1000,
              inputVAT: 13
            },
            inbound: {
              mete: 1000000,
              quantity: 50,
              unitPrice: 1,
              unitPriceExcludingVAT: 1,
              amount: 1000,
              amountExcludingVAT: 1000,
              inputVAT: 13
            },
            outbound: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            }
          },
          {
            id: 8,
            unitType: measureTypeEnum.ACCOUNTING.V,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            unitPriceExcludingVAT: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
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
            beginPeriod: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            },
            endPeriod: {
              mete: null,
              quantity: null,
              unitPrice: null,
              unitPriceExcludingVAT: null,
              amount: null,
              amountExcludingVAT: null,
              inputVAT: null
            },
            inbound: {
              mete: 600000,
              quantity: 50,
              unitPrice: 0.002,
              unitPriceExcludingVAT: 0.002,
              amount: 1200,
              amountExcludingVAT: 1200,
              inputVAT: 13
            },
            outbound: {
              mete: 600000,
              quantity: 50,
              unitPrice: 0.002,
              unitPriceExcludingVAT: 0.002,
              amount: 1200,
              amountExcludingVAT: 1200,
              inputVAT: 13
            }
          }
        ],
        totalElements: 7
      }
    }
  }
}

// 收发存详情
const getSendAndReceiveStorageDetail = {
  url: '/api/wms/report/raw-materials/send-and-receive-storage/detail',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        // material:{

        // },
        content: [
          {
            id: 1,
            boolPartyA: true,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            mete: 800000,
            requisitionsSN: 'SG-AFTER-123456',
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
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 入库单号
            }
          },
          {
            id: 2,
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
            unitPrice: 0.02,
            amount: 47100,
            amountExcludingVAT: 41681.42,
            inputVAT: 5418.58,
            requisitionsSN: 'SG-AFTER-123456',
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
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 入库单号
            }
          },
          {
            id: 3,
            specification: '57*21*3*9 * Q325B',
            classifyId: 110,
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
            amount: 9174,
            amountExcludingVAT: 8188.58,
            requisitionsSN: 'SG-AFTER-123456',
            inputVAT: 1055.42,
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
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 入库单号
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
            requisitionsSN: 'SG-AFTER-133456',
            thickness: 0.326,
            length: 3907.62,
            width: 1000,
            mete: 200000,
            weight: 1000000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.05,
            amount: 500,
            amountExcludingVAT: 450,
            inputVAT: 50,
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
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 入库单号
            }
          },
          {
            id: 6,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            mete: 80,
            unitPrice: 0.01,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
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
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
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
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 入库单号
            }
          },
          {
            id: 8,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
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
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 入库单号
            }
          }
        ]
        // , 'endPeriod', 'inbound', 'outbound'
      }
    }
  }
}

export default [getSendAndReceiveStorage, getSendAndReceiveStorageDetail]