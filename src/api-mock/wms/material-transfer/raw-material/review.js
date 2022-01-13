import { transferTypeEnum, partyAMatTransferEnum, transferCreateTypeEnum, materialFreezeTypeEnum } from '@enum-ms/wms'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { matClsEnum, rawMatClsEnum } from '@/utils/enum/modules/classification'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

// 待审核调拨单id列表
const getPendingReviewIdList = {
  url: '/api/wms/transfer/application/review/raw-materials/pending/ids',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: [6, 1, 2] // 待审核调拨单id列表
    }
  }
}

// 审核通过
const reviewPassed = {
  url: '/api/wms/transfer/application/review/raw-materials/passed',
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
  url: '/api/wms/transfer/application/review/raw-materials/returned',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 调拨审核列表
const get = {
  url: '/api/wms/transfer/application/review/raw-materials',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 6, // 调拨单id
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
            source: [
              {
                factory: {
                  id: 1,
                  name: '彩虹3号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '62号仓库'
                }
              }
            ],
            direction: {
              project: {
                // 项目
                id: 3,
                name: '长沙五一广场',
                shortName: '五一广场',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 1,
                name: '长江2号工厂'
              },
              warehouse: {
                id: 1,
                name: '662号仓库'
              }
            },
            borrowProject: {
              // 借用项目
              id: 2,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            boolBorrowReturnNotSelf: true, // 是否其他项目借用归还
            transferType: transferTypeEnum.BORROW_RETURN.V, // 调拨类型
            reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
            founderName: '@cname', // 创建人（填写调拨的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            // reviewerName: '@cname', // 审核人（审核的人）
            // reviewTime: '@datetime(T)', // 审核时间
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 1, // 调拨单id
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
            boolHasUnfreeze: true, // 是否有解冻记录
            source: [
              {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              },
              {
                project: {
                  // 项目
                  id: 3,
                  name: '长沙五一广场',
                  shortName: '五一广场',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '彩虹3号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '62号仓库'
                }
              }
            ],
            direction: {
              factory: {
                id: 1,
                name: '长江2号工厂'
              },
              warehouse: {
                id: 1,
                name: '662号仓库'
              }
            },
            transferType: transferTypeEnum.PUBLIC_WARE.V, // 调拨类型
            reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
            founderName: '@cname', // 创建人（填写调拨的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            // reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 调拨单id
            transferCreateType: transferCreateTypeEnum.OUTBOUND.V,
            basicClass: rawMatClsEnum.MATERIAL.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
            source: [
              {
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              }
            ],
            direction: {
              project: {
                // 项目
                id: 1,
                name: '你脸红个泡泡茶壶666号主路',
                shortName: '你脸红个泡泡茶壶',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 1,
                name: '长江2号工厂'
              },
              warehouse: {
                id: 1,
                name: '662号仓库'
              }
            },
            transferType: transferTypeEnum.PROJECT_WARE.V, // 调拨类型
            reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
            founderName: '@cname', // 创建人（填写调拨的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            // reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
            // reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 3, // 调拨单id
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
            source: [
              {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              }
            ],
            direction: {
              project: {
                // 项目
                id: 1,
                name: '你脸红个泡泡茶壶666号主路',
                shortName: '你脸红个泡泡茶壶',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 1,
                name: '长江2号工厂'
              },
              warehouse: {
                id: 1,
                name: '662号仓库'
              }
            },
            transferType: transferTypeEnum.PROJECT_WARE.V, // 调拨类型
            reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
            approvalComments: '尴尬咯', // 审批意见
            founderName: '@cname', // 创建人（填写调拨的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 4, // 调拨单id
            basicClass: rawMatClsEnum.MATERIAL.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
            source: [
              {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              }
            ],
            direction: {
              project: {
                // 项目
                id: 1,
                name: '你脸红个泡泡茶壶666号主路',
                shortName: '你脸红个泡泡茶壶',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 1,
                name: '长江2号工厂'
              },
              warehouse: {
                id: 1,
                name: '662号仓库'
              }
            },
            transferType: transferTypeEnum.RETURN_PARTY_A.V, // 调拨类型
            reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
            approvalComments: '咋办咯', // 审批意见
            founderName: '@cname', // 创建人（填写调拨的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 5, // 调拨单id
            basicClass: rawMatClsEnum.GAS.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
            source: [
              {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              }
            ],
            direction: {
              project: {
                // 项目
                id: 1,
                name: '你脸红个泡泡茶壶666号主路',
                shortName: '你脸红个泡泡茶壶',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 1,
                name: '长江2号工厂'
              },
              warehouse: {
                id: 1,
                name: '662号仓库'
              }
            },
            transferType: transferTypeEnum.PUBLIC_WARE.V, // 调拨类型
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            founderName: '@cname', // 创建人（填写调拨的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          }
        ],
        totalElements: 6
      }
    }
  }
}

// 调拨详情1
const detail_id1 = {
  url: '/api/wms/transfer/application/review/raw-materials/1',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 1, // 调拨单id
        basicClass: rawMatClsEnum.STEEL_PLATE.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
        source: [
          {
            project: {
              // 项目
              id: 2,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '长江1号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            project: {
              // 项目
              id: 3,
              name: '长沙五一广场',
              shortName: '五一广场',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '彩虹3号工厂'
            },
            warehouse: {
              id: 1,
              name: '62号仓库'
            }
          }
        ],
        direction: {
          factory: {
            id: 1,
            name: '长江2号工厂'
          },
          warehouse: {
            id: 1,
            name: '662号仓库'
          }
        },
        transferType: transferTypeEnum.PUBLIC_WARE.V, // 调拨类型
        reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
        list: [
          {
            id: 1,
            serialNumber: '3192520223',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            boolPartyA: true, // 甲供材料
            specification: 'Q325B',
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
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
          },
          {
            id: 2,
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specification: 'Q235B',
            quantity: 5,
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
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ],
        boolHasUnfreeze: true, // 是否有解冻记录
        // 解冻列表
        unfreezeList: [
          {
            id: 1,
            freezeType: materialFreezeTypeEnum.TRANSFER.V,
            remark: 'fffff', // 解冻备注
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
            material: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              serialNumber: /([0-9]{8})/,
              classifyId: 103,
              basicClass: matClsEnum.STEEL_PLATE.V,
              specification: 'Q325B',
              mete: 800000, // 解冻核算量
              quantity: 10, // 解冻数量
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
              }
            },
            founderName: '@cname', // 操作人
            createTime: '@datetime(T)' // 创建日期
          },
          {
            id: 2,
            freezeType: materialFreezeTypeEnum.TRANSFER.V,
            remark: '666666', // 解冻备注
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
            material: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              serialNumber: /([0-9]{8})/,
              classifyId: 103,
              basicClass: matClsEnum.STEEL_PLATE.V,
              specification: 'Q325B',
              mete: 200000, // 解冻核算量
              quantity: 2, // 解冻数量
              thickness: 10,
              length: 1000,
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
                id: 2,
                name: '二号工厂',
                shortName: '二工'
              },
              warehouse: {
                id: 1,
                name: '667号仓库'
              }
            },
            founderName: '@cname', // 操作人
            createTime: '@datetime(T)' // 创建日期
          }
        ]
      }
    }
  }
}

// 调拨详情2
const detail_id2 = {
  url: '/api/wms/transfer/application/review/raw-materials/2',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 2, // 调拨单id
        basicClass: rawMatClsEnum.MATERIAL.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
        source: [
          {
            factory: {
              id: 1,
              name: '长江1号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ],
        direction: {
          project: {
            // 项目
            id: 1,
            name: '你脸红个泡泡茶壶666号主路',
            shortName: '你脸红个泡泡茶壶',
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          },
          factory: {
            id: 1,
            name: '长江2号工厂'
          },
          warehouse: {
            id: 1,
            name: '662号仓库'
          }
        },
        transferType: transferTypeEnum.PROJECT_WARE.V, // 调拨类型
        reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
        list: [
          {
            id: 1,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            boolPartyA: true, // 甲供材料
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
          },
          {
            id: 2,
            classifyId: 247,
            basicClass: matClsEnum.MATERIAL.V,
            boolPartyA: true, // 甲供材料
            quantity: 10,
            brand: '啊哈',
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

// 调拨详情3
const detail_id3 = {
  url: '/api/wms/transfer/application/review/raw-materials/3',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 3, // 调拨单id
        basicClass: rawMatClsEnum.STEEL_PLATE.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
        source: [
          {
            project: {
              // 项目
              id: 2,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '长江1号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ],
        direction: {
          project: {
            // 项目
            id: 1,
            name: '你脸红个泡泡茶壶666号主路',
            shortName: '你脸红个泡泡茶壶',
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          },
          factory: {
            id: 1,
            name: '长江2号工厂'
          },
          warehouse: {
            id: 1,
            name: '662号仓库'
          }
        },
        transferType: transferTypeEnum.PROJECT_WARE.V, // 调拨类型
        reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
        list: [
          {
            id: 1,
            serialNumber: '3192520223',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            boolPartyA: true, // 甲供材料
            partyATransferType: partyAMatTransferEnum.BORROW.V, // 甲供调拨类型
            specification: 'Q325B',
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            remark: '66666',
            mete: 800000,
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
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

const detail_id4 = {
  url: '/api/wms/transfer/application/review/raw-materials/4',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 5, // 调拨单id
        basicClass: rawMatClsEnum.GAS.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
        source: [
          {
            factory: {
              id: 1,
              name: '长江1号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ],
        direction: {
          project: {
            // 项目
            id: 1,
            name: '你脸红个泡泡茶壶666号主路',
            shortName: '你脸红个泡泡茶壶',
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          },
          factory: {
            id: 1,
            name: '长江2号工厂'
          },
          warehouse: {
            id: 1,
            name: '662号仓库'
          }
        },
        transferType: transferTypeEnum.RETURN_PARTY_A.V, // 调拨类型
        reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
        list: [
          {
            id: 1,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            boolPartyA: true, // 甲供材料
            brand: '嘻嘻',
            remark: '66666',
            mete: 10,
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

// 调拨详情5
const detail_id5 = {
  url: '/api/wms/transfer/application/review/raw-materials/5',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 5, // 调拨单id
        basicClass: rawMatClsEnum.GAS.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
        invoiceType: invoiceTypeEnum.SPECIAL.V,
        taxRate: 13,
        source: [
          {
            factory: {
              id: 1,
              name: '长江1号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ],
        direction: {
          project: {
            // 项目
            id: 1,
            name: '你脸红个泡泡茶壶666号主路',
            shortName: '你脸红个泡泡茶壶',
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          },
          factory: {
            id: 1,
            name: '长江2号工厂'
          },
          warehouse: {
            id: 1,
            name: '662号仓库'
          }
        },
        transferType: transferTypeEnum.PROJECT_WARE.V, // 调拨类型
        reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
        list: [
          {
            id: 1,
            classifyId: 901,
            // specification: '',
            basicClass: matClsEnum.GAS.V,
            boolPartyA: true, // 甲供材料
            partyATransferType: partyAMatTransferEnum.BUY_IN.V, // 甲供调拨类型
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 222,
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

// 调拨详情6
const detail_id6 = {
  url: '/api/wms/transfer/application/review/raw-materials/6',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 1, // 调拨单id
        basicClass: rawMatClsEnum.STEEL_PLATE.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
        source: [
          {
            factory: {
              id: 1,
              name: '彩虹3号工厂'
            },
            warehouse: {
              id: 1,
              name: '62号仓库'
            }
          }
        ],
        direction: {
          project: {
            // 项目
            id: 3,
            name: '长沙五一广场',
            shortName: '五一广场',
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          },
          factory: {
            id: 1,
            name: '长江2号工厂'
          },
          warehouse: {
            id: 1,
            name: '662号仓库'
          }
        },
        borrowProject: {
          // 借用项目
          id: 2,
          name: '长安街666666号辅路',
          shortName: '长安街',
          serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
        },
        boolBorrowReturnNotSelf: true, // 是否其他项目借用归还
        transferType: transferTypeEnum.BORROW_RETURN.V, // 调拨类型
        reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
        list: [
          {
            id: 1,
            serialNumber: '3192520223',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            boolPartyA: true, // 甲供材料
            specification: 'Q325B',
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
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
          },
          {
            id: 2,
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specification: 'Q235B',
            quantity: 5,
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

export default [
  get,
  reviewReturned,
  reviewPassed,
  getPendingReviewIdList,
  detail_id1,
  detail_id2,
  detail_id3,
  detail_id4,
  detail_id5,
  detail_id6
]
