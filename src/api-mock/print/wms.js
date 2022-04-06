import { matClsEnum } from '@/utils/enum/modules/classification'
import { weightMeasurementModeEnum } from '@/utils/enum/modules/finance'
import { logisticsPayerEnum } from '@/utils/enum/modules/logistics'
import { materialOutboundModeEnum, measureTypeEnum } from '@/utils/enum/modules/wms'
import { patternLicensePlate } from '@/utils/validate/pattern'

// 出库单
const wmsRmOutboundReceipt = {
  url: RegExp('/api/wms/outbound/receipt/' + '[1-9][0-9]?' + '/print'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        header: {
          outboundSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
          reviewerName: '@cname', // 出库办理人
          printerName: '@cname', // 打印人
          printDate: '@datetime(T)',
          outboundTime: '@datetime(T)'
        },
        table: [
          {
            id: 1,
            boolPartyA: true, // 甲供材料
            materialOutboundMode: materialOutboundModeEnum.HALF.V, // 物料出库方式
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            remark: '66666',
            mete: 800000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          },
          {
            id: 2,
            boolPartyA: true, // 甲供材料
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specification: 'Q235B',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 5,
            thickness: 20,
            length: 1500,
            width: 2000,
            brand: '哈哈',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 2355000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          },
          {
            id: 3,
            specification: '57*21*3*9 * Q325B',
            classifyId: 110,
            nationalStandard: 'GB-10', // 国家标准
            basicClass: matClsEnum.SECTION_STEEL.V,
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 1,
            length: 10000,
            totalLength: 10,
            brand: '马钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 152900,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            recipient: undefined,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 4,
              name: '668号仓库'
            }
          }
        ]
      }
    }
  }
}

// 入库单
const wmsRmInboundReceipt = {
  url: RegExp('/api/wms/inbound/receipt/' + '[1-9][0-9]?' + '/print'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        header: {
          inboundSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 人库单号
          purchaseSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 采购单号
          weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 计量方式
          logisticsPayerType: logisticsPayerEnum.SUPPLIER.V, // 物流费用承担
          supplierName: '@cname', // 供应商名称
          reviewerName: '@cname', // 出库办理人
          printerName: '@cname', // 打印人
          printDate: '@datetime(T)',
          inboundTime: '@datetime(T)',
          licensePlate: patternLicensePlate // 车牌号
          // project: {
          //   id: 1,
          //   name: '长安街666666号辅路',
          //   shortName: '长安街',
          //   serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          // }
        },
        table: [
          {
            id: 1,
            serialNumber: '3192520223',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            remark: '66666',
            mete: 822222,
            rejectMete: 222222,
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
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
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
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ]
      }
    }
  }
}

// 退库单
const wmsRmReturnReceipt = {
  url: RegExp('/api/wms/return/receipt/' + '[1-9][0-9]?' + '/print'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        header: {
          returnSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
          reviewerName: '@cname', // 退库审核人
          applicantName: '@cname', // 退库申请人
          printerName: '@cname', // 打印人
          printDate: '@datetime(T)',
          returnTime: '@datetime(T)'
        },
        table: [
          {
            id: 1,
            boolPartyA: true, // 甲供材料
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            remark: '66666',
            mete: 800000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 2,
            boolPartyA: true, // 甲供材料
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specification: 'Q235B',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 5,
            thickness: 20,
            length: 1500,
            width: 2000,
            brand: '哈哈',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 2355000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 3,
            specification: '57*21*3*9 * Q325B',
            classifyId: 110,
            nationalStandard: 'GB-10', // 国家标准
            basicClass: matClsEnum.SECTION_STEEL.V,
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 1,
            length: 10000,
            totalLength: 10,
            brand: '马钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 152900,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '萧山工厂'
            },
            warehouse: {
              id: 4,
              name: '668号仓库'
            }
          }
        ]
      }
    }
  }
}

export default [wmsRmInboundReceipt, wmsRmOutboundReceipt, wmsRmReturnReceipt]
