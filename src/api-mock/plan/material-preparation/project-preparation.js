import { projectPreparationMatClsEnum } from '@/utils/enum/modules/classification'
import { preparationRangeEnum } from '@/utils/enum/modules/plan'
import { componentTypeEnum } from '@/utils/enum/modules/building-steel'

// 获取项目备料列表
const get = {
  url: '/api/plan/material-preparation/project-preparation',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1, // 备料单id
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 备料单号
            project: {
              // 项目信息
              id: 1,
              name: '长安街号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            monomer: {
              // 单体信息
              id: 1,
              name: '1号楼'
            },
            area: {
              // 区域信息
              id: 1,
              name: '1区'
            },
            preparationRangeType: preparationRangeEnum.AREA.V, // 备料范围
            listType: componentTypeEnum.STRUCTURE.V, // 清单类型
            materialBasicClass: projectPreparationMatClsEnum.STEEL_PLATE.V | projectPreparationMatClsEnum.SECTION_STEEL.V, // 物料种类，清单中对应的类型
            listMete: 10000, // 清单量
            preparationMete: 5000, // 备料量
            listUploaderNames: ['@cname', '@cname'], // 清单上传人
            preparationUpdaterName: '@cname', // 备料更新人
            boolPrepared: true, // 备料状态（true:已备料，false:未备料）
            listUpdateTime: '@dateTime(T)', // 清单更新时间
            preparationUpdateTime: '@dateTime(T)', // 备料更新时间
            createTime: '@dateTime(T)' // 创建时间
          },
          {
            id: 2, // 备料单id
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 备料单号
            project: {
              // 项目信息
              id: 1,
              name: '浙大科技园',
              shortName: '科技园',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            monomer: {
              // 单体信息
              id: 1,
              name: 'A楼'
            },
            preparationRangeType: preparationRangeEnum.MONOMER.V, // 备料范围
            listType: componentTypeEnum.STRUCTURE.V, // 清单类型
            materialBasicClass: projectPreparationMatClsEnum.STEEL_PLATE.V, // 物料种类，清单中对应的类型
            listMete: 20000, // 清单量
            preparationMete: 0, // 备料量
            listUploaderNames: ['@cname'], // 清单上传人
            preparationUpdaterName: '@cname', // 备料更新人
            boolPrepared: false, // 备料状态（true:已备料，false:未备料）
            listUpdateTime: '@dateTime(T)', // 清单更新时间
            preparationUpdateTime: null, // 备料更新时间
            createTime: '@dateTime(T)' // 创建时间
          },
          {
            id: 3, // 备料单id
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 备料单号
            project: {
              // 项目信息
              id: 1,
              name: '浙大科技园',
              shortName: '科技园',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            monomer: {
              // 单体信息
              id: 1,
              name: 'A楼'
            },
            preparationRangeType: preparationRangeEnum.MONOMER.V, // 备料范围
            listType: componentTypeEnum.ENCLOSURE.V, // 清单类型
            materialBasicClass: projectPreparationMatClsEnum.STEEL_COIL.V, // 物料种类，清单中对应的类型
            listMete: 10000, // 清单量
            preparationMete: 0, // 备料量
            listUploaderNames: ['@cname'], // 清单上传人
            preparationUpdaterName: '@cname', // 备料更新人
            boolPrepared: false, // 备料状态（true:已备料，false:未备料）
            listUpdateTime: '@dateTime(T)', // 清单更新时间
            preparationUpdateTime: null, // 备料更新时间
            createTime: '@dateTime(T)' // 创建时间
          },
          {
            id: 4, // 备料单id
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 备料单号
            project: {
              // 项目信息
              id: 1,
              name: '骑士办公园区',
              shortName: '办公园区',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            preparationRangeType: preparationRangeEnum.PROJECT.V, // 备料范围
            listType: componentTypeEnum.AUXILIARY_MATERIAL.V, // 清单类型
            materialBasicClass: projectPreparationMatClsEnum.MATERIAL.V, // 物料种类，清单中对应的类型
            listMete: 20000, // 清单量
            preparationMete: 0, // 备料量
            listUploaderNames: ['@cname'], // 清单上传人
            preparationUpdaterName: '@cname', // 备料更新人
            boolPrepared: false, // 备料状态（true:已备料，false:未备料）
            listUpdateTime: '@dateTime(T)', // 清单更新时间
            preparationUpdateTime: null, // 备料更新时间
            createTime: '@dateTime(T)' // 创建时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 获取项目备料列表详情
const detail_1 = {
  url: '/api/plan/material-preparation/project-preparation/1',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 1, // 备料单id
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 备料单号
        project: {
          // 项目信息
          id: 1,
          name: '长安街号辅路',
          shortName: '长安街',
          serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
        },
        monomer: {
          // 单体信息
          id: 1,
          name: '1号楼'
        },
        area: {
          // 区域信息
          id: 1,
          name: '1区'
        },
        preparationRangeType: preparationRangeEnum.AREA.V, // 备料范围
        listType: componentTypeEnum.STRUCTURE.V, // 清单类型
        materialBasicClass: projectPreparationMatClsEnum.STEEL_PLATE.V | projectPreparationMatClsEnum.SECTION_STEEL.V, // 物料种类，清单中对应的类型
        listMete: 10000, // 清单量
        preparationMete: 5000, // 备料量
        listUploaderNames: ['@cname', '@cname'], // 清单上传人
        preparationUpdaterName: '@cname', // 备料更新人
        boolPrepared: true, // 备料状态（true:已备料，false:未备料）
        listUpdateTime: '@dateTime(T)', // 清单更新时间
        preparationUpdateTime: '@dateTime(T)', // 备料更新时间
        createTime: '@dateTime(T)', // 创建时间
        // 技术清单
        technologyList: [
          {
            classifyName: '钢板'
          }
        ]
      }
    }
  }
}

// 编辑项目备料信息
const edit = {
  url: '/api/plan/material-preparation/project-preparation',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取未完工的项目备料范围列表
const getProjectListForRangeInfo = {
  url: '/api/plan/material-preparation/project-preparation/projects/range-info',
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
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 合同编号
            name: '一号项目工程',
            shortName: '一号项目',
            boolStrucPrepared: false, // 结构备料状态（true:已备料，false:未备料,备料后，若备料数据被清空属于未备料）
            boolEnclPrepared: false, // 围护备料状态
            boolAuxPrepared: false, // 辅材备料状态
            strucPreparationRangeType: undefined, // 结构备料范围
            enclPreparationRangeType: undefined, // 围护备料范围
            auxPreparationRangeType: undefined // 辅材备料范围
          },
          {
            id: 2,
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 合同编号
            name: '二号项目工程',
            shortName: '二号项目',
            boolStrucPrepared: true, // 结构备料状态（true:已备料，false:未备料）
            boolEnclPrepared: true, // 围护备料状态
            boolAuxPrepared: true, // 辅材备料状态
            strucPreparationRangeType: preparationRangeEnum.MONOMER.V, // 结构备料范围
            enclPreparationRangeType: preparationRangeEnum.MONOMER.V, // 围护备料范围
            auxPreparationRangeType: preparationRangeEnum.MONOMER.V // 辅材备料范围
          },
          {
            id: 3,
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 合同编号
            name: '三号项目工程',
            shortName: '三号项目',
            boolStrucPrepared: false, // 结构备料状态（true:已备料，false:未备料）
            boolEnclPrepared: false, // 围护备料状态
            boolAuxPrepared: false, // 辅材备料状态
            strucPreparationRangeType: preparationRangeEnum.AREA.V, // 结构备料范围
            enclPreparationRangeType: preparationRangeEnum.AREA.V, // 围护备料范围
            auxPreparationRangeType: preparationRangeEnum.AREA.V// 辅材备料范围
          },
          {
            id: 4,
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 合同编号
            name: '四号项目工程',
            shortName: '四号项目',
            boolStrucPrepared: false, // 结构备料状态（true:已备料，false:未备料）
            boolEnclPrepared: false, // 围护备料状态
            boolAuxPrepared: false, // 辅材备料状态
            strucPreparationRangeType: preparationRangeEnum.PROJECT.V, // 结构备料范围
            enclPreparationRangeType: undefined, // 围护备料范围
            auxPreparationRangeType: undefined// 辅材备料范围
          }
        ]
      }
    }
  }
}

// 设置未完工的项目备料范围列表
const setProjectListForRangeInfo = {
  url: '/api/plan/material-preparation/project-preparation/projects/range-info',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [get, edit, detail_1, getProjectListForRangeInfo, setProjectListForRangeInfo]
