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
            underReviewMete: 0, // 审核中的核算量
            quantity: 10,
            returnedQuantity: 0,
            underReviewQuantity: 0, // 审核中的数量
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
            // returnTime: '@datetime(T)', // 归还时间（归还调拨审核通过时间）
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
            // returnTime: '@datetime(T)', // 归还时间（归还调拨审核通过）
            transferorName: '@cname', // 调拨人
            transferTime: '@datetime(T)' // 调拨时间
          }
        ],
        totalElements: 5
      }
    }
  }
}

// 审核通过
const returnMaterial = {
  url: '/api/wms/transfer/party-a/borrow/return',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

/**
 * 根据借用id获取可归还的物料列表
 * 根据配置获取
 * 在配置可从其他项目归还的情况下，能查询到其他项目的材料（这其中不包含“甲供”材料以及被借用项目的材料）
 * 未配置从其他项目归还的情况下，只查询当前项目库（可查询出“甲供”材料）和公共库
 * @param {*} id
 * @returns
 */
const getReturnableMatListById = {
  url: RegExp('/api/wms/transfer/party-a/borrow/' + '[1-9][0-9]*' + '/return-mat-list'),
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
            boolPartyA: false, // 甲供材料
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000, // 核算量
            frozenMete: 400000, // 冻结量
            quantity: 10, // 数量
            frozenQuantity: 5, // 冻结数量
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            project: {
              id: 3,
              name: '五一街项目',
              shortName: '五一街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 1,
              name: '一号车间',
              shortName: '一工'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 2,
            boolPartyA: false, // 甲供材料
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 500000, // 核算量
            frozenMete: 400000, // 冻结量
            quantity: 5, // 数量
            frozenQuantity: 4, // 冻结数量
            thickness: 10,
            length: 1500,
            width: 1500,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            project: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '你脸红个泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 1,
              name: '一号车间',
              shortName: '一工'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 3,
            boolPartyA: false, // 甲供材料
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 500000, // 核算量
            frozenMete: 400000, // 冻结量
            quantity: 5, // 数量
            frozenQuantity: 4, // 冻结数量
            thickness: 10,
            length: 1500,
            width: 1500,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            workshop: {
              id: 1,
              name: '一号车间',
              shortName: '一工'
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

export default [get, returnMaterial, getReturnableMatListById]
