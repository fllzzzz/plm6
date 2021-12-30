import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { patternLicensePlate } from '@/utils/validate/pattern'
import { matClsEnum, rawMatClsEnum } from '@/utils/enum/modules/classification'

// 待审核退库单id列表
const getPendingReviewIdList = {
  url: '/api/wms/return/application/review/raw-materials/pending/ids',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: [1, 2, 3] // 待审核退库单id列表
    }
  }
}

// 审核通过
const reviewPassed = {
  url: '/api/wms/return/application/review/raw-materials/passed',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 审核退回
const reviewReturned = {
  url: '/api/wms/return/application/review/raw-materials/returned',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 退库审核列表
const get = {
  url: '/api/wms/return/application/review/raw-materials',
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
            basicClass: 7, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            supplier: {
              // 供应商
              id: 1,
              name: '杭州天天向上有限公司'
            },
            reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
            founderName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            // reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 退库单id
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            supplier: {
              // 供应商
              id: 1,
              name: '杭州艾哈有限公司'
            },
            reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
            approvalComments: '@csentence',
            founderName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 3, // 退库单id
            basicClass: matClsEnum.GAS.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            supplier: {
              // 供应商
              id: 1,
              name: '杭州艾哈有限公司'
            },
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            approvalComments: '@csentence',
            founderName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 4, // 退库单id
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 退库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            supplier: {
              // 供应商
              id: 1,
              name: '吖丫丫有限公司'
            },
            reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
            approvalComments: '@csentence',
            founderName: '@cname', // 创建人（填写退库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          }
        ],
        totalElements: 4
      }
    }
  }
}

// 钢材详情
const detail_id1 = {
  url: '/api/wms/return/application/review/raw-materials/1',
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
          //   id: 3,
          //   sn: '110_0',
          //   specification: '57*21*3*9 * Q325B',
          //   classifyId: 110,
          //   basicClass: 2,
          //   quantity: 1,
          //   length: 10000,
          //   totalLength: 10,
          //   brand: '马钢',
          //   heatNoAndBatchNo: 'ooopp',
          //   mete: 152900,
          //   weight: 152900,
          //   unitPrice: 0.03,
          //   amount: 4587,
          //   amountExcludingVAT: 4059.29,
          //   requisitionsSN: 'SG-AFTER-123456',
          //   inputVAT: 527.71,
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

// 气体详情
const detail_id2 = {
  url: '/api/wms/return/application/review/raw-materials/2',
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
        loadingWeight: 2000.0, // 过磅重量
        list: [
          {
            id: 1,
            classifyId: 204,
            specification: 'M27 * 60',
            color: '天蓝',
            basicClass: 1,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 800000,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
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

// 辅材详情
const detail_id3 = {
  url: '/api/wms/return/application/review/raw-materials/3',
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
        loadingWeight: 2000.0, // 过磅重量
        list: [
          {
            id: 1,
            classifyId: 901,
            // specification: '',
            basicClass: 1,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 250000,
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
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

export default [get, reviewReturned, reviewPassed, getPendingReviewIdList, detail_id1, detail_id2, detail_id3]
