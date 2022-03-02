import { matClsEnum } from '@/utils/enum/modules/classification'

// 钢板库存
const getSteelPlate = {
  url: '/api/wms/material-inventory/steel-plate',
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
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 型材库存
const getSectionSteel = {
  url: '/api/wms/material-inventory/section-steel',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 4,
            classifyId: 110,
            basicClass: matClsEnum.SECTION_STEEL.V,
            nationalStandard: 'GB-06', // 国家标准
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
          }
        ],
        totalElements: 1
      }
    }
  }
}

// 钢卷库存
const getSteelCoil = {
  url: '/api/wms/material-inventory/steel-coil',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 5,
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
            }
          }
        ],
        totalElements: 1
      }
    }
  }
}

// 辅材库存
const getAuxMatInventory = {
  url: '/api/wms/material-inventory/auxiliary-material',
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
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            frozenMete: 1, // 冻结量
            frozenQuantity: 1, // 冻结数量
            brand: '嘻嘻',
            remark: '66666',
            mete: 10,
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 4,
              name: '622号仓库'
            }
          },
          {
            id: 1,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            frozenMete: 1, // 冻结量
            frozenQuantity: 1, // 冻结数量
            brand: '嘻嘻',
            remark: '66666',
            mete: 10,
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
            }
          }
        ],
        totalElements: 1
      }
    }
  }
}

// 气体库存
const getGasInventory = {
  url: '/api/wms/material-inventory/gas',
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
            classifyId: 901,
            // specification: '',
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 250000,
            frozenMete: 25000, // 冻结量
            frozenQuantity: 1, // 冻结数量
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 4,
              name: '622号仓库'
            }
          }
        ],
        totalElements: 1
      }
    }
  }
}

export default [getSteelPlate, getSectionSteel, getSteelCoil, getAuxMatInventory, getGasInventory]
