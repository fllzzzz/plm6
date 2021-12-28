import { ElMessage } from 'element-plus'

import { getDarkColor } from '@/utils/color'
import { deepClone, isNotBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

export default function useCommonCalc({ cu, form, basicClass }) {
  // 计算最大重量
  function calcMaxMete(row) {
    if (row.quantity) {
      row.maxMete = row.source.singleReturnableMete * row.quantity
    } else {
      row.maxMete = row.source.singleReturnableMete
    }
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
    const mete = {}
    const length = {}
    const sourceKV = {}
    form.list.forEach((v) => {
      if (row.id === v.id) {
        // 做转换，避免草稿或修改与退库列表不一致
        if (sourceKV[v.source.id]) {
          v.source = sourceKV[v.source.id]
        } else {
          sourceKV[v.source.id] = v.source
        }
        if (isNotBlank(mete[v.id])) {
          mete[v.id] += v.mete || 0
          length[v.id] += v.length || 0
        } else {
          mete[v.id] = v.mete || 0
          length[v.id] = v.length || 0
        }
      }
    })
    Object.keys(sourceKV).forEach((key) => {
      const sourceMaterial = sourceKV[key]
      sourceMaterial.returnableMete = sourceMaterial.sourceReturnableMete - (mete[sourceMaterial.id] || 0)
      if (basicClass === rawMatClsEnum.SECTION_STEEL.V) {
        sourceMaterial.returnableLength = sourceMaterial.sourceReturnableLength - (length[sourceMaterial.id] || 0)
      }
    })
  }

  // 校验是否超出原材料的可退库量
  function checkOverSource(row) {
    calcReturnInfo(row)
    let showFlag = row.source.returnableMete < 0 && !row.overTipColor
    let unshowFlag = row.source.returnableMete >= 0 && row.overTipColor

    // 型材还需校验长度
    if (row.basicClass === rawMatClsEnum.SECTION_STEEL.V) {
      showFlag = (row.source.returnableMete < 0 || row.source.returnableLength < 0) && !row.overTipColor
      unshowFlag = row.source.returnableMete >= 0 && row.source.returnableLength >= 0 && row.overTipColor
    }
    if (showFlag) {
      const overTipColor = getDarkColor()
      row.overTipColor = overTipColor
      form.list.forEach((r) => {
        if (r.id === row.id) {
          r.overTipColor = overTipColor
        }
      })
    }
    if (unshowFlag) {
      form.list.forEach((r) => {
        if (r.id === row.id) {
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
      let quantityChange = false
      if (row.source.quantity < row.quantity) {
        quantityChange = true
        row.quantity = row.source.quantity
        _row.maxQuantity = row.source.quantity
      }
      // 计算最大重量
      calcMaxMete(row)
      // 旧值与新值比较，避免数量变化产生量的问题
      if (row.maxMete < _row.mete || quantityChange) {
        _row.maxMete = row.maxMete
        overList.push(_row)
      }
    })
    if (overList.length) {
      cu.props.abnormalList = overList
      ElMessage.warning('退库记录的可退库信息发生变化，已自动修正，可查看异常列表')
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
