import { matClsEnum, projectPreparationMatClsEnum, rawMatClsEnum } from '@/utils/enum/modules/classification'
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
            technologyListType: componentTypeEnum.STRUCTURE.V, // 清单类型
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
            technologyListType: componentTypeEnum.STRUCTURE.V, // 清单类型
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
            technologyListType: componentTypeEnum.ENCLOSURE.V, // 清单类型
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
            technologyListType: componentTypeEnum.AUXILIARY_MATERIAL.V, // 清单类型
            materialBasicClass: projectPreparationMatClsEnum.MATERIAL.V, // 物料种类，清单中对应的类型
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
            id: 5, // 备料单id
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 备料单号
            project: {
              // 项目信息
              id: 1,
              name: '骑士办公园区',
              shortName: '办公园区',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            preparationRangeType: preparationRangeEnum.PROJECT.V, // 备料范围
            technologyListType: componentTypeEnum.AUXILIARY_MATERIAL.V, // 清单类型
            materialBasicClass: projectPreparationMatClsEnum.MATERIAL.V, // 物料种类，清单中对应的类型
            // listMete: 20000, // 清单量
            // preparationMete: 0, // 备料量
            listUploaderNames: ['@cname'], // 清单上传人
            preparationUpdaterName: '@cname', // 备料更新人
            boolPrepared: false, // 备料状态（true:已备料，false:未备料）
            listUpdateTime: '@dateTime(T)', // 清单更新时间
            preparationUpdateTime: null, // 备料更新时间
            createTime: '@dateTime(T)' // 创建时间
          }
        ],
        totalElements: 6
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
        technologyListType: componentTypeEnum.STRUCTURE.V, // 清单类型
        withoutList: false, // 是否无清单备料
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
            steelClassifyConfId: 1, // 钢材分类配置id
            steelClassifyConfName: '钢板', // 钢材配置名称
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 主科目分类（列如：钢板\型材\钢卷）
            material: 'Q325B', // 材质
            specification: '10', // 规格（型材）/厚度（钢板）
            listMete: 10000 // 清单量
          },
          {
            steelClassifyConfId: 1, // 钢材分类配置id
            steelClassifyConfName: '钢板', // 钢材配置名称
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 主科目分类（列如：钢板\型材\钢卷）
            material: 'Q235B', // 材质
            specification: '11', // 规格（型材）/厚度（钢板）
            listMete: 20000 // 清单量
          },
          {
            steelClassifyConfId: 4, // 钢材分类配置id
            steelClassifyConfName: '工字钢', // 钢材配置名称
            basicClass: rawMatClsEnum.SECTION_STEEL.V, // 主科目分类（列如：钢板\型材\钢卷）
            material: 'Q325B', // 材质
            specification: '10*10*200*500', // 规格（型材）/厚度（钢板）
            listMete: 6666.66 // 清单量
          }
        ],
        inventoryList: [
          {
            id: 1, // 记录id
            remark: undefined, // 备注
            material: {
              id: 1, // 材料id
              boolPartyA: false, // 甲供材料
              boolHasFrozen: true, // 有冻结
              classifyId: 103,
              basicClass: rawMatClsEnum.STEEL_PLATE.V,
              specification: 'Q325B',
              thickness: 9.6, // 厚度
              theoryThickness: 10, // 理论厚度
              length: 11000,
              width: 990,
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
              mete: 800000, // 仓库核算量
              frozenMete: 400000, // 仓库冻结量
              quantity: 10, // 仓库数量
              frozenQuantity: 8, // 仓库冻结数量
              usedQuantity: 3, // 当前备料单利用的数量
              usedMete: 300000, // 当前备料单利用的核算量
              projectUsedQuantity: 5, // 项目利用数量
              projectUsedMete: 500000, // 项目利用数量
              projectOutboundUsedQuantity: 3, // 项目利用且已经出库的数量
              projectOutboundUsedMete: 300000 // 项目利用且已经出库的数量
            }
          }
        ],
        purchaseList: [
          {
            id: 1, // 记录id
            remark: '须购买鞍钢', // 备注
            material: {
              classifyId: 103,
              basicClass: rawMatClsEnum.STEEL_PLATE.V,
              specification: 'Q325B',
              thickness: 10, // 厚度
              theoryThickness: 10, // 理论厚度
              length: 11000, // 长度
              width: 990, // 宽度
              brand: '嘻嘻', // 品牌
              mete: 1000000, // 需要采购核算量
              sortingMete: 100000, // 分拣量
              quantity: 10, // 需要采购数量
              sortingQuantity: 1 // 分拣数量
            }
          }
        ]
      }
    }
  }
}

// 详情-钢板/型材-无清单备料
const detail_2 = {
  url: '/api/plan/material-preparation/project-preparation/2',
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
        technologyListType: componentTypeEnum.STRUCTURE.V, // 清单类型
        withoutList: true, // 是否无清单备料
        materialBasicClass: projectPreparationMatClsEnum.STEEL_PLATE.V, // 物料种类，清单中对应的类型
        listMete: 20000, // 清单量
        preparationMete: 0, // 备料量
        listUploaderNames: ['@cname', '@cname'], // 清单上传人
        preparationUpdaterName: undefined, // 备料更新人
        boolPrepared: false, // 备料状态（true:已备料，false:未备料）
        listUpdateTime: '@dateTime(T)', // 清单更新时间
        preparationUpdateTime: undefined, // 备料更新时间
        createTime: '@dateTime(T)', // 创建时间
        // 技术清单
        technologyList: [
          {
            steelClassifyConfId: 1, // 钢材分类配置id
            steelClassifyConfName: '钢板', // 钢材配置名称
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 主科目分类（列如：钢板\型材\钢卷）
            material: 'Q325B', // 材质
            specification: '10', // 规格（型材）/厚度（钢板）
            listMete: 10000 // 清单量
          }
        ],
        inventoryList: [],
        purchaseList: []
      }
    }
  }
}

// 详情-配套件-有清单备料
const detail_5 = {
  url: '/api/plan/material-preparation/project-preparation/5',
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
          name: '骑士办公园区',
          shortName: '办公园区',
          serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
        },
        preparationRangeType: preparationRangeEnum.PROJECT.V, // 备料范围
        technologyListType: componentTypeEnum.AUXILIARY_MATERIAL.V, // 清单类型
        withoutList: false, // 是否无清单备料
        materialBasicClass: projectPreparationMatClsEnum.MATERIAL.V, // 物料种类，清单中对应的类型
        // listMete: 20000, // 清单量
        // preparationMete: 0, // 备料量
        listUploaderNames: ['@cname'], // 清单上传人
        preparationUpdaterName: undefined, // 备料更新人
        boolPrepared: false, // 备料状态（true:已备料，false:未备料）
        listUpdateTime: '@dateTime(T)', // 清单更新时间
        preparationUpdateTime: undefined, // 备料更新时间
        createTime: '@dateTime(T)', // 创建时间
        // 技术清单
        technologyList: [
          {
            id: 1, // 技术清单id
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            remark: '66666',
            listQuantity: 10,
            listMete: 10
          },
          {
            id: 2, // 技术清单id
            classifyId: 247,
            basicClass: matClsEnum.MATERIAL.V,
            brand: '彩虹',
            color: '天蓝色',
            remark: '66666',
            listQuantity: 10,
            listMete: 10
          }
        ],
        inventoryList: [],
        purchaseList: []
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
            strucWithoutList: false, // 结构-是否无清单备料
            enclWithoutList: false, // 围护-是否无清单备料
            auxWithoutList: false, // 辅材-是否无清单备料
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
            strucWithoutList: true, // 结构-是否无清单备料
            enclWithoutList: true, // 围护-是否无清单备料
            auxWithoutList: true, // 辅材-是否无清单备料
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
            strucWithoutList: true, // 结构-是否无清单备料
            enclWithoutList: true, // 围护-是否无清单备料
            auxWithoutList: true, // 辅材-是否无清单备料
            boolStrucPrepared: false, // 结构备料状态（true:已备料，false:未备料）
            boolEnclPrepared: false, // 围护备料状态
            boolAuxPrepared: false, // 辅材备料状态
            strucPreparationRangeType: preparationRangeEnum.AREA.V, // 结构备料范围
            enclPreparationRangeType: preparationRangeEnum.AREA.V, // 围护备料范围
            auxPreparationRangeType: preparationRangeEnum.MONOMER.V // 辅材备料范围
          },
          {
            id: 4,
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 合同编号
            name: '四号项目工程',
            shortName: '四号项目',
            strucWithoutList: false, // 结构-是否无清单备料
            enclWithoutList: false, // 围护-是否无清单备料
            auxWithoutList: false, // 辅材-是否无清单备料
            boolStrucPrepared: false, // 结构备料状态（true:已备料，false:未备料）
            boolEnclPrepared: false, // 围护备料状态
            boolAuxPrepared: false, // 辅材备料状态
            strucPreparationRangeType: preparationRangeEnum.PROJECT.V, // 结构备料范围
            enclPreparationRangeType: undefined, // 围护备料范围
            auxPreparationRangeType: undefined // 辅材备料范围
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

export default [get, edit, detail_1, detail_2, detail_5, getProjectListForRangeInfo, setProjectListForRangeInfo]
