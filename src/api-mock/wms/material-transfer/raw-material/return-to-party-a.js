import { rawMatClsEnum } from '@/utils/enum/modules/classification'

// 归还甲方，记录
const get = {
  url: '/api/wms/transfer/return-to-party-a',
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
            basicClass: rawMatClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000,
            quantity: 10,
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
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            transfer: {
              id: 1, // 调拨id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 借用调拨单号
            },
            applicantName: '@cname', // 调拨人
            createTime: '@datetime(T)' // 调拨时间
          },
          {
            id: 2,
            classifyId: 103,
            basicClass: rawMatClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000,
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
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            transfer: {
              id: 1, // 调拨id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 借用调拨单号
            },
            applicantName: '@cname', // 调拨人
            createTime: '@datetime(T)' // 调拨时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

export default [get]
