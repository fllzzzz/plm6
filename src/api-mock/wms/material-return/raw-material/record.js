import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { matClsEnum, rawMatClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'

// 退库申请列表
const get = {
  url: '/api/wms/return/application/record/raw-materials',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1, // 退库单id
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 退库物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            // 项目
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            // 出库单列表
            outboundList: [
              {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              {
                id: 2,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
            editable: true, // 可修改的
            applicantName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            // reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 退库单id
            basicClass: rawMatClsEnum.SECTION_STEEL.V, // 退库物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            // 项目id
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            // 出库单列表
            outboundList: [
              {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            editable: true, // 可修改的
            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 3, // 退库单id
            basicClass: rawMatClsEnum.STEEL_COIL.V, // 退库物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            // 出库单列表
            outboundList: [
              {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            editable: true, // 可修改的
            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 4, // 退库单id
            basicClass: rawMatClsEnum.MATERIAL.V, // 退库物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            editable: true, // 可修改的
            // 出库单列表
            outboundList: [
              {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 5, // 退库单id
            basicClass: rawMatClsEnum.GAS.V, // 退库物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            editable: true, // 可修改的
            // 出库单列表
            outboundList: [
              {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ],
            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 钢材详情
const detail_id1 = {
  url: '/api/wms/return/application/record/raw-materials/1',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 1, // 退库单id
        basicClass: rawMatClsEnum.STEEL_PLATE.V, // 退库物料基础类型
        reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
        createTime: '@datetime(T)', // 创建时间
        applicant: {
          // 创建人（填写退库的人）
          name: '@cname',
          deptName: '仓库'
        },
        list: [
          {
            id: 1,
            serialNumber: '3192520223',
            classifyId: 103,
            basicClass: rawMatClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            quantity: 3,
            mete: 800000,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            remark: '66666',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
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
            source: {
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
              length: 2000,
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
            }
          },
          {
            id: 2,
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specification: 'Q235B',
            quantity: 3,
            thickness: 20,
            length: 1500,
            width: 2000,
            brand: '哈哈',
            heatNoAndBatchNo: 'fddfd',
            mete: 2355000,
            weight: 2355000,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
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
            source: {
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
              length: 3000,
              width: 3000,
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
          }
        ]
      }
    }
  }
}

// 型材详情
const detail_id2 = {
  url: '/api/wms/return/application/record/raw-materials/2',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 2, // 退库单id
        reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
        basicClass: matClsEnum.SECTION_STEEL.V, // 退库物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
        createTime: '@datetime(T)', // 创建时间
        applicant: {
          // 创建人（填写退库的人）
          name: '@cname',
          deptName: '仓库'
        },
        list: [
          {
            id: 1,
            specification: '57*21*3*9 * Q325B',
            classifyId: 110,
            nationalStandard: 'GB-10', // 国家标准
            basicClass: matClsEnum.SECTION_STEEL.V,
            length: 10000,
            brand: '马钢',
            heatNoAndBatchNo: 'ooo-pp',
            quantity: 1,
            mete: 152900,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '1号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            source: {
              id: 1,
              classifyId: 110,
              nationalStandard: 'GB-10', // 国家标准
              boolPartyA: false, // 是否甲供材料
              outboundUnitType: measureTypeEnum.MEASURE.V, // 出库单位类型
              basicClass: rawMatClsEnum.SECTION_STEEL.V,
              specification: '57*21*3*9 * Q325B',
              mete: 800000,
              returnableMete: 600000,
              singleMete: 800000, // 单件重量
              singleReturnableMete: 600000, // 单件可退库重量
              quantity: 1,
              length: 10000,
              singleReturnableLength: 8000,
              brand: '嘻嘻',
              heatNoAndBatchNo: 'ooo-pp',
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
          }
        ]
      }
    }
  }
}

// 型材详情
const detail_id3 = {
  url: '/api/wms/return/application/record/raw-materials/3',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 2, // 退库单id
        reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
        basicClass: matClsEnum.STEEL_COIL.V, // 退库物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
        createTime: '@datetime(T)', // 创建时间
        applicant: {
          // 创建人（填写退库的人）
          name: '@cname',
          deptName: '仓库'
        },
        list: [
          {
            id: 1,
            classifyId: 120,
            outboundUnitType: measureTypeEnum.MEASURE.V, // 出库单位类型
            basicClass: rawMatClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            width: 1000,
            mete: 1000000,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '1号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            source: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              classifyId: 120,
              outboundUnitType: measureTypeEnum.MEASURE.V, // 出库单位类型
              basicClass: rawMatClsEnum.STEEL_COIL.V,
              specification: 'DC51D+Z',
              quantity: 2207,
              color: '天蓝',
              brand: '武钢',
              heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
              thickness: 0.326,
              width: 1000,
              mete: 1000000,
              returnableMete: 700000,
              singleMete: 1000000, // 单件重量(钢卷mete = singleMete)
              singleReturnableMete: 700000, // 单件可退库重量
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
          }
        ]
      }
    }
  }
}
// 辅材详情
const detail_id4 = {
  url: '/api/wms/return/application/record/raw-materials/4',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 2, // 退库单id
        reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
        basicClass: matClsEnum.MATERIAL.V, // 退库物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
        createTime: '@datetime(T)', // 创建时间
        applicant: {
          // 创建人（填写退库的人）
          name: '@cname',
          deptName: '仓库'
        },
        list: [
          {
            id: 1,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: rawMatClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            remark: '66666',
            mete: 101,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
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
            source: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              outboundUnitType: measureTypeEnum.ACCOUNTING.V, // 出库单位类型
              classifyId: 204,
              specification: 'M27 * 60',
              unitNet: 1, // 单位净量
              basicClass: rawMatClsEnum.MATERIAL.V,
              brand: '嘻嘻',
              remark: '66666',
              mete: 100,
              returnableMete: 100,
              singleMete: 100, // 单件重量
              singleReturnableMete: 100, // 单件可退库重量
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
          },
          {
            id: 2,
            boolPartyA: false, // 是否甲供材料
            outboundUnitType: measureTypeEnum.MEASURE.V, // 出库单位类型
            classifyId: 247,
            color: '天蓝',
            basicClass: rawMatClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            remark: '66666',
            unitNet: 10000, // 单位净量
            quantity: 11,
            mete: 110000,
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
            source: {
              id: 2,
              boolPartyA: false, // 是否甲供材料
              outboundUnitType: measureTypeEnum.MEASURE.V, // 出库单位类型
              classifyId: 247,
              color: '天蓝',
              basicClass: rawMatClsEnum.MATERIAL.V,
              quantity: 10,
              brand: '嘻嘻',
              remark: '66666',
              unitNet: 10000, // 单位净量
              mete: 100000,
              returnableMete: 100000,
              singleMete: 10000, // 单件重量
              singleReturnableMete: 10000, // 单件可退库重量
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
          }
        ]
      }
    }
  }
}

// 气体详情
const detail_id5 = {
  url: '/api/wms/return/application/record/raw-materials/5',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 3, // 退库单id
        reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
        basicClass: matClsEnum.GAS.V, // 退库物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
        createTime: '@datetime(T)', // 创建时间
        applicant: {
          // 创建人（填写退库的人）
          name: '@cname',
          deptName: '仓库'
        },
        list: [
          {
            id: 1,
            classifyId: 901,
            basicClass: rawMatClsEnum.GAS.V,
            quantity: 5,
            brand: '嘻嘻',
            remark: '66666',
            unitNet: 25000, // 单位净量
            mete: 125000,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
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
            source: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              outboundUnitType: measureTypeEnum.ACCOUNTING.V, // 出库单位类型
              classifyId: 901,
              basicClass: rawMatClsEnum.GAS.V,
              quantity: 4,
              brand: '嘻嘻',
              remark: '66666',
              unitNet: 25000, // 单位净量
              mete: 100000,
              returnableMete: 100000,
              singleMete: 25000, // 单件重量
              singleReturnableMete: 25000, // 单件可退库重量
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
          }
        ]
      }
    }
  }
}

// 修改采购合同
const edit = {
  url: '/api/wms/return/application/record/raw-materials',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除采购合同
const del = {
  url: '/api/wms/return/application/record/raw-materials',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [get, edit, del, detail_id1, detail_id2, detail_id3, detail_id4, detail_id5]
