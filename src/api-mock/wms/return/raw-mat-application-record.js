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
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id

            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
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
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
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
            reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
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
            basicClass: 1,
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
            basicClass: 1,
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
          // {
          //   uid: 4,
          //   // sn: '110_1',
          //   classifyId: 110,
          //   quantity: 2,
          //   length: 2500,
          //   totalLength: 5,
          //   brand: '鞍钢',
          //   heatNoAndBatchNo: 'qqwww',
          //   requisitionsSN: 'SG-AFTER-123456',
          //   mete: 150000,
          //   weight: 150000,
          //   unitPrice: 0.04,
          //   amount: 6000,
          //   amountExcludingVAT: 5000,
          //   inputVAT: 1000,
          //   project: {
          //     'id|+1': 1,
          //     'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
          //     'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
          //     serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          //   },
          //   factoryId: 1,
          //   warehouse: {
          //     id: 1,
          //     name: '666号仓库'
          //   }
          // },
          // {
          //   id: 5,
          //   classifyId: 120,
          //   basicClass: 4,
          //   specification: 'DC51D+Z',
          //   quantity: 1,
          //   color: '天蓝',
          //   brand: '武钢',
          //   heatNoAndBatchNo: '12341234fsafs1234',
          //   requisitionsSN: 'SG-AFTER-133456',
          //   thickness: 0.326,
          //   length: 3907.62,
          //   width: 1000,
          //   mete: 10000,
          //   weight: 10000,
          //   unitPrice: 0.05,
          //   amount: 500,
          //   amountExcludingVAT: 450,
          //   inputVAT: 50,
          //   project: {
          //     'id|+1': 1,
          //     'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
          //     'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
          //     serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          //   },
          //   factoryId: 1,
          //   warehouse: {
          //     id: 4,
          //     name: '668号仓库'
          //   }
          // }
        ]
      }
    }
  }
}

// 型钢详情
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
            basicClass: 2,
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

// 型钢详情
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
            mete: 100000,
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
// 详情
// const detail_id2 = {
//   url: '/api/wms/return/application/record/raw-materials/2',
//   method: 'get',
//   timeout: 1000,
//   response: () => {
//     return {
//       code: 20000,
//       message: '成功',
//       data: {
//         id: 2, // 退库单id
//         reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
//         basicClass: matClsEnum.MATERIAL.V, // 退库物料基础类型
//         serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
//         loadingWeight: 2000.0, // 过磅重量
//         list: [
//           {
//             id: 1,
//             classifyId: 204,
//             specification: 'M27 * 60',
//             color: '天蓝',
//             basicClass: 1,
//             quantity: 10,
//             brand: '嘻嘻',
//             remark: '66666',
//             mete: 800000,
//             unitPrice: 0.01,
//             amount: 8000,
//             amountExcludingVAT: 7079.64,
//             inputVAT: 920.36,
//             requisitionsSN: 'SG-AFTER-123456',
//             project: {
//               'id|+1': 1,
//               'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
//               'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
//               serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
//             },
//             factoryId: 1,
//             warehouse: {
//               id: 1,
//               name: '666号仓库'
//             }
//           }
//         ]
//       }
//     }
//   }
// }

// // 辅材详情
// const detail_id3 = {
//   url: '/api/wms/return/application/record/raw-materials/3',
//   method: 'get',
//   timeout: 1000,
//   response: () => {
//     return {
//       code: 20000,
//       message: '成功',
//       data: {
//         id: 3, // 退库单id
//         reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
//         basicClass: matClsEnum.GAS.V, // 退库物料基础类型
//         serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
//         loadingWeight: 2000.0, // 过磅重量
//         list: [
//           {
//             id: 1,
//             classifyId: 901,
//             // specification: '',
//             basicClass: 1,
//             quantity: 10,
//             brand: '嘻嘻',
//             remark: '66666',
//             mete: 250000,
//             unitPrice: 0.01,
//             amount: 8000,
//             amountExcludingVAT: 7079.64,
//             inputVAT: 920.36,
//             requisitionsSN: 'SG-AFTER-123456',
//             project: {
//               'id|+1': 1,
//               'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
//               'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
//               serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/,
//             },
//             factoryId: 1,
//             warehouse: {
//               id: 1,
//               name: '666号仓库',
//             },
//           },
//         ],
//       },
//     }
//   },
// }

// 修改采购订单
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

// 删除采购订单
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

export default [get, edit, del, detail_id1, detail_id2, detail_id3]
