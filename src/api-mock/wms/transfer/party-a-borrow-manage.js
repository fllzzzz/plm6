import { matClsEnum } from '@/utils/enum/modules/classification'
import { borrowReturnStatusEnum } from '@/utils/enum/modules/wms'

const get = {
  url: '/api/wms/transfer/party-a/borrow',
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
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000,
            returnedMete: 0,
            quantity: 10,
            returnedQuantity: 0,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            remark: '66666',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            borrowProject: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '你脸红个泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            returnStatus: borrowReturnStatusEnum.NOT_RETURNED.V,
            borrowTransfer: {
              id: 1, // 借用调拨id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 借用调拨单号
            },

            // returnTransfers: [
            // 按时间倒序排列
            //   {
            //     id: 4, // 归还调拨id
            //     serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 归还调拨单号
            //   },
            //   {
            //     id: 6, // 归还调拨id
            //     serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 归还调拨单号
            //   }
            // ],
            // returneeTime: '@datetime(T)', // 归还时间（归还调拨审核通过时间）
            transferorName: '@cname', // 调拨人
            transferTime: '@datetime(T)' // 调拨时间
          },
          {
            id: 2,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000,
            returnedMete: 800000,
            quantity: 10,
            returnedQuantity: 10,
            thickness: 10,
            length: 1000,
            width: 1200,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            remark: '66666',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            borrowProject: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '你脸红个泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            returnStatus: borrowReturnStatusEnum.RETURNED.V,
            borrowTransfer: {
              id: 1, // 借用调拨id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 借用调拨单号
            },
            returnTransfers: [
              // 按时间倒序排列
              {
                id: 4, // 归还调拨id
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 归还调拨单号
              },
              {
                id: 6, // 归还调拨id
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 归还调拨单号
              }
            ],
            // returneeTime: '@datetime(T)', // 归还时间（归还调拨审核通过）
            transferorName: '@cname', // 调拨人
            transferTime: '@datetime(T)' // 调拨时间
          }
        ],
        totalElements: 5
      }
    }
  }
}

export default [get]
