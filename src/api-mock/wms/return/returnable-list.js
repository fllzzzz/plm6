import { rawMatClsEnum } from '@/utils/enum/modules/classification'

// 钢材废料列表
const getSteelList = {
  url: '/api/wms/return/returnable',
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
            boolPartyA: false, // 甲供列表
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

export default [getSteelList]
