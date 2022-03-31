import { matClsEnum } from '@/utils/enum/modules/classification'
import { materialOutboundModeEnum, measureTypeEnum } from '@/utils/enum/modules/wms'

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
            factoryId: 1,
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
            factoryId: 1,
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
            factoryId: 1,
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
            factoryId: 1,
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

export default [wmsRmOutboundReceipt]
