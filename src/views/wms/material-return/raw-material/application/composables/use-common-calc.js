import { ElMessage } from 'element-plus'

import { getDarkColor } from '@/utils/color'
import { deepClone, isNotBlank, toPrecision } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

export default function useCommonCalc({ cu, form, basicClass, baseUnit }) {
  // 计算最大重量
  function calcMaxMete(row) {
    if (basicClass !== rawMatClsEnum.STEEL_COIL.V) {
      if (row.quantity) {
        row.maxMete = row.source.singleReturnableMete * row.quantity
      } else {
        row.maxMete = row.source.singleReturnableMete
      }
    } else {
      row.maxMete = row.source.singleReturnableMete
    }
    row.maxQuantity = row.source.quantity
  }

  // 提取退库材料相同的对象
  function extractSource(list) {
    const sourceKV = {}
    list.forEach((v) => {
      if (sourceKV[v.source.id]) {
        v.source = sourceKV[v.source.id]
      } else {
        sourceKV[v.source.id] = v.source
      }
    })
  }

  // 计算退库信息
  function calcReturnInfo(row) {
    const mete = {} // 核算量
    const quantity = {} // 计量量
    const length = {} // 长度
    const sourceKV = {} // 退库源材料，key：id，value: info
    form.list.forEach((v) => {
      if (row.source.id === v.source.id) {
        const sourceId = v.source.id
        // 做转换，避免草稿或修改与退库列表不一致
        if (sourceKV[sourceId]) {
          v.source = sourceKV[sourceId]
        } else {
          sourceKV[sourceId] = v.source
        }
        if (isNotBlank(mete[sourceId])) {
          quantity[sourceId] += v.quantity || 0
          mete[sourceId] += v.mete || 0
          length[sourceId] += v.length * Number(v.quantity) || 0
        } else {
          quantity[sourceId] = v.quantity || 0
          mete[sourceId] = v.mete || 0
          length[sourceId] = v.length * Number(v.quantity) || 0
        }
      }
    })
    Object.keys(sourceKV).forEach((key) => {
      const sourceMaterial = sourceKV[key]
      sourceMaterial.returnableMete = sourceMaterial.sourceReturnableMete - (mete[sourceMaterial.id] || 0)
      if (sourceMaterial.measureUnit) {
        sourceMaterial.returnableQuantity = (sourceMaterial.quantity || 0) - (quantity[sourceMaterial.id] || 0)
      }
      if (basicClass === rawMatClsEnum.SECTION_STEEL.V) {
        sourceMaterial.returnableLength = sourceMaterial.sourceReturnableLength - (length[sourceMaterial.id] || 0)
      }
    })
  }

  // 校验是否超出原材料的可退库量
  function checkOverSource(row) {
    calcReturnInfo(row)
    const returnableMete = toPrecision(row.source.returnableMete, row.source.accountingPrecision)
    // 需要展示
    let showFlag = returnableMete < 0
    // 不需要展示
    let unshowFlag = returnableMete >= 0

    // 有核算单位的物料需要校验可退库数量
    if (row.source.measureUnit) {
      const returnableQuantity = toPrecision(row.source.returnableQuantity, row.source.measurePrecision)
      showFlag = showFlag || returnableQuantity < 0
      unshowFlag = unshowFlag && returnableQuantity >= 0
    }

    // 型材还需校验长度
    if (row.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
      const returnableLength = toPrecision(row.source.returnableLength, baseUnit.value ? baseUnit.value.length.precision : 0)
      showFlag = showFlag || returnableLength < 0
      unshowFlag = unshowFlag && returnableLength >= 0
    }

    showFlag = !!(showFlag && !row.overTipColor)
    unshowFlag = !!(unshowFlag && row.overTipColor)
    if (showFlag) {
      const overTipColor = getDarkColor()
      row.overTipColor = overTipColor
      form.list.forEach((r) => {
        if (r.source.id === row.source.id) {
          r.overTipColor = overTipColor
        }
      })
    }
    if (unshowFlag) {
      form.list.forEach((r) => {
        if (r.source.id === row.source.id) {
          r.overTipColor = undefined
        }
      })
    }
  }

  // 初始校验超出最大量
  function initCheckOverMaxWeight(list) {
    const overList = []
    list.forEach((row) => {
      const _row = deepClone(row)
      // 检测数量是否发生变化
      let quantityChange = false
      // 没有计量单位的不做处理
      if (row.measureUnit) {
        if (row.source.quantity < row.quantity) {
          quantityChange = true
          row.quantity = row.source.quantity
          _row.maxQuantity = row.source.quantity
        }
      }

      // 检测重量是否发生变化
      // 计算最大重量
      calcMaxMete(row)

      let meteChange = false
      // 旧值与新值比较，避免数量变化产生量的问题
      if (row.maxMete < _row.mete) {
        meteChange = true
        _row.maxMete = row.maxMete
      }
      // 钢板长宽理论上不会发生错误的情况，代码处理是以防万一
      // 检测长度是否发生变化
      let lengthChange = false
      if (row.basicClass === rawMatClsEnum.STEEL_PLATE.V) {
        if (row.source.length < row.length) {
          lengthChange = true
          _row.maxLength = row.source.length
          // 清空长度
          row.length = undefined
        }
      }

      if (row.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
        if (row.source.singleReturnableLength < row.length) {
          lengthChange = true
          _row.maxLength = row.source.singleReturnableLength
          // 清空长度
          row.length = undefined
        }
      }

      // 检测宽度是否发生变化
      let widthChange = false
      if (row.basicClass === rawMatClsEnum.STEEL_PLATE.V) {
        if (row.source.width < row.width) {
          widthChange = true
          _row.maxWidth = row.source.width
          // 清空宽度
          row.width = undefined
        }
      }

      // 发生异常
      if (meteChange || quantityChange || lengthChange || widthChange) {
        overList.push(_row)
        // 清空当前的数量、核算量
        row.quantity = undefined
        row.mete = undefined
      }
    })
    if (overList.length) {
      cu.props.abnormalList = overList
      ElMessage.warning('退库记录的可退库信息发生变化，已清除关联信息，具体信息查看异常列表')
    }
  }

  return {
    calcMaxMete,
    extractSource,
    calcReturnInfo,
    checkOverSource,
    initCheckOverMaxWeight
  }
}
