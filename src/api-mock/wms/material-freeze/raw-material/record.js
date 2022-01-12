import { materialFreezeTypeEnum } from '@/utils/enum/modules/wms'

const get = {
  url: '/api/wms/freeze/raw-material',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            boolPartyA: false, // 是否甲供材料
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000, // 核算量
            frozenMete: 600000, // 冻结量
            quantity: 10, // 数量
            frozenQuantity: 7, // 冻结数量
            thickness: 10,
            length: 1000,
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
            },
            recordList: [
              {
                id: 1,
                freezeType: materialFreezeTypeEnum.REQUISITIONS.V,
                document: {
                  id: 1,
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                project: {
                  id: 1,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                quantity: 2,
                operatorName: '@cname',
                frozenTime: '@datetime(T)'
              },
              {
                id: 2,
                freezeType: materialFreezeTypeEnum.OUTBOUND.V,
                document: {
                  id: 1,
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                project: {
                  id: 1,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                quantity: 2,
                mete: 1000,
                operatorName: '@cname',
                frozenTime: '@datetime(T)'
              },
              {
                id: 3,
                freezeType: materialFreezeTypeEnum.TRANSFER.V,
                document: {
                  id: 1,
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                project: {
                  id: 1,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                quantity: 3,
                mete: 1000,
                operatorName: '@cname',
                frozenTime: '@datetime(T)'
              }
            ]
          },
          {
            id: 2,
            boolPartyA: false, // 是否甲供材料
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
            },
            recordList: [
              {
                id: 1,
                freezeType: materialFreezeTypeEnum.REQUISITIONS.V,
                document: {
                  id: 1,
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                project: {
                  id: 1,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                quantity: 5,
                mete: 1000,
                operatorName: '@cname',
                frozenTime: '@datetime(T)'
              }
            ]
          },
          {
            id: 3,
            boolPartyA: false, // 是否甲供材料
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 500000, // 核算量
            frozenMete: 500000, // 冻结量
            quantity: 5, // 数量
            frozenQuantity: 5, // 冻结数量
            thickness: 10,
            length: 1500,
            width: 2000,
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
            },
            recordList: [
              {
                id: 1,
                freezeType: materialFreezeTypeEnum.REQUISITIONS.V,
                document: {
                  id: 1,
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                project: {
                  id: 1,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                quantity: 5,
                mete: 1000,
                operatorName: '@cname',
                frozenTime: '@datetime(T)'
              }
            ]
          }
        ],
        totalElements: 3
      }
    }
  }
}

// 解冻
const unfreezeHandling = {
  url: '/api/wms/freeze/raw-material/unfreeze',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

const getMaterialFreezeRecordById = {
  url: '/api/wms/freeze/raw-material/record',
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
            freezeType: materialFreezeTypeEnum.REQUISITIONS.V,
            document: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            quantity: 2,
            mete: 1000,
            operatorName: '@cname',
            frozenTime: '@datetime(T)'
          },
          {
            id: 2,
            freezeType: materialFreezeTypeEnum.OUTBOUND.V,
            document: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            quantity: 2,
            mete: 1000,
            operatorName: '@cname',
            frozenTime: '@datetime(T)'
          },
          {
            id: 3,
            freezeType: materialFreezeTypeEnum.TRANSFER.V,
            document: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            quantity: 3,
            mete: 1000,
            operatorName: '@cname',
            frozenTime: '@datetime(T)'
          }
        ]
      }
    }
  }
}

export default [get, unfreezeHandling, getMaterialFreezeRecordById]
