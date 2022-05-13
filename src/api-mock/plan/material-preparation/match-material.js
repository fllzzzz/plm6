import { matClsEnum } from '@/utils/enum/modules/classification'
import { materialIsWholeEnum } from '@/utils/enum/modules/wms'

// 钢板库存
const getMatchSteelPlateList = {
  url: '/api/wms/material-preparation/match/steel-plate',
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
            thickness: 9.5,
            theoryThickness: 10, // 理论厚度
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
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 500000, // 核算量
            quantity: 5, // 数量
            thickness: 9.6,
            theoryThickness: 10, // 理论厚度
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
            id: 3,
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            materialIsWhole: materialIsWholeEnum.ODDMENT.V, // 物料类型（整料|余料）
            specification: 'Q325B',
            mete: 50000, // 核算量
            quantity: 1, // 数量
            thickness: 9.6,
            theoryThickness: 10, // 理论厚度
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
        totalElements: 3
      }
    }
  }
}

// 型材库存
const getMatchSectionSteel = {
  url: '/api/wms/material-preparation/match/section-steel',
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
            nationalStandard: 'GB-10', // 国家标准
            specification: '10*10*200*500 * Q325B',
            quantity: 10,
            frozenQuantity: 5, // 冻结数量
            mete: 1000000,
            frozenMete: 500000, // 冻结量
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            length: 6000,
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
const getMatchSteelCoil = {
  url: '/api/wms/material-preparation/match/steel-coil',
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
const getMatchAuxMat = {
  url: '/api/wms/material-preparation/match/auxiliary-material',
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
          }
        ],
        totalElements: 1
      }
    }
  }
}

// // 气体库存
// const getGasInventory = {
//   url: '/api/wms/material-preparation/match/gas',
//   method: 'get',
//   timeout: 1000,
//   response: () => {
//     return {
//       code: 20000,
//       message: '成功',
//       data: {
//         content: [
//           {
//             id: 1,
//             classifyId: 901,
//             // specification: '',
//             basicClass: matClsEnum.GAS.V,
//             quantity: 10,
//             brand: '嘻嘻',
//             remark: '66666',
//             mete: 250000,
//             frozenMete: 25000, // 冻结量
//             frozenQuantity: 1, // 冻结数量
//             factory: {
//               id: 1,
//               name: '一号工厂',
//               shortName: '一工'
//             },
//             warehouse: {
//               id: 4,
//               name: '622号仓库'
//             }
//           }
//         ],
//         totalElements: 1
//       }
//     }
//   }
// }

export default [getMatchSteelPlateList, getMatchSectionSteel, getMatchSteelCoil, getMatchAuxMat]
