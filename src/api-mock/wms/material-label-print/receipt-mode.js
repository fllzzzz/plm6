import { matClsEnum } from '@/utils/enum/modules/classification'
import { receiptTypeEnum } from '@/utils/enum/modules/wms'

// 获取物料打印标签列表-物料模式
const get = {
  url: '/api/wms/material/label-print/receipt-mode',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        content: [
          {
            id: 1,
            basicClass: matClsEnum.STEEL_PLATE.V,
            receipt: {
              id: 1,
              receiptType: receiptTypeEnum.INBOUND.V,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 单据编号
            },
            // 项目id
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            pendingPrintedMaterialNumber: 0,
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)'
          },
          {
            id: 2,
            basicClass: matClsEnum.STEEL_PLATE.V,
            receipt: {
              id: 1,
              receiptType: receiptTypeEnum.OUTBOUND.V,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 单据编号
            },
            // 项目id
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            'pendingPrintedMaterialNumber|1-10': 1,
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)'
          },
          {
            id: 3,
            basicClass: matClsEnum.STEEL_PLATE.V,
            receipt: {
              id: 1,
              receiptType: receiptTypeEnum.TRANSFER.V,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 单据编号
            },
            // 项目id
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            'pendingPrintedMaterialNumber|1-10': 1,
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)'
          },
          {
            id: 4,
            basicClass: matClsEnum.STEEL_PLATE.V,
            receipt: {
              id: 1,
              receiptType: receiptTypeEnum.RETURN.V,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 单据编号
            },
            // 项目id
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            'pendingPrintedMaterialNumber|1-10': 1,
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)'
          }
        ],
        hasNextPage: false,
        totalElements: 14
      },
      message: '操作成功'
    }
  }
}

const detail_1 = {
  url: '/api/wms/material/label-print/receipt-mode/1',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        id: 1,
        pendingPrintedMaterialNumber: 0,
        basicClass: matClsEnum.STEEL_PLATE.V,
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 单据编号
        receiptId: 1,
        receiptType: receiptTypeEnum.INBOUND.V,
        // 项目id
        'projects|2': [
          {
            'id|+1': 1,
            'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
            'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          }
        ],
        'pendingPrintedMaterialNumber|1-10': 1,
        reviewerName: '@cname', // 审核人（审核的人）
        createTime: '@datetime(T)',
        materialList: [
          {
            id: 1,
            barcode: 100000000001,
            boolPartyA: false, // 甲供材料
            boolHasFrozen: true, // 有冻结
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000, // 核算量
            frozenMete: 400000, // 冻结量
            quantity: 10, // 数量
            frozenQuantity: 5, // 冻结数量
            thickness: 10,
            length: 11000,
            width: 990,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            projectFrozen: [
              // 针对不同项目可出库的冻结数量（目前只针对申购冻结）
              { projectId: 1, quantity: 2, mete: 200000 }
            ],
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 2,
            barcode: 111000000001,
            boolPartyA: true, // 甲供材料
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 500000, // 核算量
            quantity: 5, // 数量
            thickness: 10,
            length: 15000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 4,
            barcode: 200000000001,
            classifyId: 110,
            basicClass: matClsEnum.SECTION_STEEL.V,
            nationalStandard: 'GB-10', // 国家标准
            specification: '10*10*200*500 * Q325B',
            quantity: 10,
            frozenQuantity: 5, // 冻结数量
            mete: 10000,
            frozenMete: 5000, // 冻结量
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            length: 3907.62,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 4,
              name: '668号仓库'
            }
          },
          {
            id: 5,
            barcode: 300000000001,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 3907,
            frozenMete: 5000, // 冻结量
            frozenQuantity: 2000, // 冻结数量
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            project: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '你脸红个泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 4,
              name: '622号仓库'
            },
            printedNumber: 1
          }
        ]
      }
    }
  }
}

const detail_2 = {
  url: '/api/wms/material/label-print/receipt-mode/2',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        id: 1,
        basicClass: matClsEnum.STEEL_PLATE.V,
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 单据编号
        receiptId: 1,
        receiptType: receiptTypeEnum.INBOUND.V,
        // 项目id
        'projects|2': [
          {
            'id|+1': 1,
            'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
            'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          }
        ],
        'pendingPrintedMaterialNumber|1-10': 1,
        reviewerName: '@cname', // 审核人（审核的人）
        createTime: '@datetime(T)',
        materialList: [
          {
            id: 5,
            barcode: 100000000001,
            boolPartyA: false, // 甲供材料
            boolHasFrozen: true, // 有冻结
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q235B',
            mete: 800000, // 核算量
            frozenMete: 400000, // 冻结量
            quantity: 10, // 数量
            frozenQuantity: 5, // 冻结数量
            thickness: 10,
            length: 10000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            projectFrozen: [
              // 针对不同项目可出库的冻结数量（目前只针对申购冻结）
              { projectId: 1, quantity: 2, mete: 200000 }
            ],
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂',
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

export default [get, detail_1, detail_2]
