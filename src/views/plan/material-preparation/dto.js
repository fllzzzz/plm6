import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { operationTypeEnum } from '@/utils/enum/modules/common'
import { isNotBlank } from '@data-type/index'

// 备料提交DTO
export const preparationSubmitDTO = async (form) => {
  const dto = {
    id: form.id
  }

  // 无清单备料，则需要上传清单
  if (form.withoutList) {
    if (Array.isArray(form.technologyList)) {
      dto.technologyList = form.technologyList.map((item) => {
        return {
          id: item.operateType === operationTypeEnum.ADD.V ? undefined : item.id, // id
          operateType: item.operateType, // 操作类型
          steelClassifyConfId: item.steelClassifyConfId, // 零件钢材配置id
          listMete: item.listMete, // 清单量
          material: item.material, // 材质
          specification: item.specification, // 规格/厚度
          basicClass: item.basicClass, // 基础分类
          steelClassifyConfName: item.steelClassifyConfName// 零件钢材名称
        }
      })
    } else {
      dto.technologyList = undefined
    }
  }

  if (isNotBlank(form.inventoryList)) {
    // 库存列表
    dto.inventoryList = form.inventoryList.map((item) => {
      const { material } = item
      return {
        id: item.boolAdd ? undefined : item.id, // 记录id
        remark: item.remark, // 备注
        material: {
          id: material.id, // 物料仓id
          classifyId: material.classifyId, // 科目id
          basicClass: material.basicClass, // 基础科目
          usedQuantity: material.usedQuantity, // 使用数量
          accountingUnit: material.accountingUnit, // 核算单位
          accountingPrecision: material.accountingPrecision, // 核算单位精度
          measureUnit: material.measureUnit, // 计量单位
          measurePrecision: material.measurePrecision, // 计量单位精度
          brand: material.brand // 品牌，若该绑定物料已有利用数量，则品牌不可变更
        }
      }
    })

    // 数值转换
    await numFmtByBasicClass(form.inventoryList, {
      prefix: 'material',
      toNum: true,
      toSmallest: true
    })
  }

  if (isNotBlank(form.purchaseList)) {
    // 采购列表
    dto.purchaseList = form.purchaseList.map((item) => {
      const { material } = item
      return {
        id: item.boolAdd ? undefined : item.id, // 记录id
        remark: item.remark, // 备注
        material: {
          classifyId: material.classifyId, // 科目id
          basicClass: material.basicClass, // 基础科目
          accountingUnit: material.accountingUnit, // 核算单位
          accountingPrecision: material.accountingPrecision, // 核算单位精度
          measureUnit: material.measureUnit, // 计量单位
          measurePrecision: material.measurePrecision, // 计量单位精度
          brand: material.brand, // 品牌，若该绑定物料已有利用数量，则品牌不可变更
          serialNumber: material.serialNumber, // 科目-规格的材料唯一编号
          specKV: material.specKV, // 规格 k:id,v:info
          specNameKV: material.specNameKV, // 规格 k:name,v:info
          specification: material.specification, // 规格
          theoryThickness: material.theoryThickness, // 理论厚度
          thickness: material.thickness, // 厚
          width: material.width, // 宽
          length: material.length, // 长
          // theoryTotalWeight: material.theoryTotalWeight, // 理论总重
          // theoryWeight: material.theoryWeight, // 理论重量
          mete: material.theoryTotalWeight, // 核算量
          quantity: material.quantity // 数量
        }
      }
    })

    // 数值转换
    await numFmtByBasicClass(dto.purchaseList, {
      prefix: 'material',
      toNum: true,
      toSmallest: true
    })
  }
  return dto
}
