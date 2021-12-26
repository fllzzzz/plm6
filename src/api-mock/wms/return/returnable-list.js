import { rawMatClsEnum } from '@/utils/enum/modules/classification'

// 钢材废料列表
const getSteelPlateList = {
  url: '/api/wms/return/returnable/1',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            classifyId: 103,
            boolPartyA: true, // 甲供列表
            basicClass: rawMatClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000,
            returnableMete: 600000,
            singleMete: 800000, // 单件重量
            singleReturnableMete: 600000, // 单件可退库重量
            quantity: 1,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            outbound: {
              id: 1,
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
            recipientName: '@cname', // 领用人
            createTime: '@datetime(T)' // 生成时间
          },
          {
            id: 2,
            classifyId: 103,
            basicClass: rawMatClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 8000000,
            returnableMete: 8000000,
            singleMete: 800000,
            singleReturnableMete: 800000,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1200,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            outbound: {
              id: 1,
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
            recipientName: '@cname', // 领用人
            createTime: '@datetime(T)' // 生成时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 型材废料列表
const getSectionSteelList = {
  url: '/api/wms/return/returnable/2',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            classifyId: 110,
            boolPartyA: false, // 甲供列表
            basicClass: rawMatClsEnum.SECTION_STEEL.V,
            specification: '57*21*3*9 * Q325B',
            mete: 800000,
            returnableMete: 600000,
            singleMete: 800000, // 单件重量
            singleReturnableMete: 600000, // 单件可退库重量
            quantity: 1,
            length: 10000,
            singleReturnableLength: 8000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            outbound: {
              id: 1,
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
            recipientName: '@cname', // 领用人
            createTime: '@datetime(T)' // 生成时间
          },
          {
            id: 2,
            classifyId: 110,
            boolPartyA: true, // 甲供列表
            basicClass: rawMatClsEnum.SECTION_STEEL.V,
            specification: '57*21*3*9 * Q325B',
            mete: 800000,
            returnableMete: 600000,
            singleMete: 800000, // 单件重量
            singleReturnableMete: 600000, // 单件可退库重量
            quantity: 1,
            length: 10000,
            singleReturnableLength: 10000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            outbound: {
              id: 1,
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
            recipientName: '@cname', // 领用人
            createTime: '@datetime(T)' // 生成时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 型材废料列表
const getSteelCoilList = {
  url: '/api/wms/return/returnable/4',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            boolPartyA: false, // 甲供列表
            classifyId: 120,
            basicClass: rawMatClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 1000000,
            returnableMete: 700000,
            singleMete: 1000000, // 单件重量(钢卷mete = singleMete)
            singleReturnableMete: 700000, // 单件可退库重量
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            outbound: {
              id: 1,
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
            recipientName: '@cname', // 领用人
            createTime: '@datetime(T)' // 生成时间
          },
          {
            id: 2,
            boolPartyA: false, // 甲供列表
            classifyId: 120,
            basicClass: rawMatClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 2207,
            color: '海蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 1000000,
            returnableMete: 1000000,
            singleMete: 1000000, // 单件重量(钢卷mete = singleMete)
            singleReturnableMete: 1000000, // 单件可退库重量
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            outbound: {
              id: 1,
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
            recipientName: '@cname', // 领用人
            createTime: '@datetime(T)' // 生成时间
          }
        ],
        totalElements: 2
      }
    }
  }
}
export default [getSteelPlateList, getSectionSteelList, getSteelCoilList]
