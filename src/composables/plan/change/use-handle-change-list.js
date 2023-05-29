import { arr2obj, obj2arr } from '@/utils/convert/type'
import { toPrecision, isNotBlank } from '@/utils/data-type'
import { changeTypeEnum, assembleOperateTypeEnum } from '@/components-system/plan/change/common.js'

export default function useHandleChangeList() {
  return {
    handleAssembleList,
    handleCompareAssembleList,
    handleComparePartList
  }
}

// 处理部件列表
function handleAssembleList(oldList, newList) {
  // 需要处理的列表，过滤完全相同的
  const needHandleOldList = []
  const needHandleOldObj = {}
  const needHandleNewList = []
  const needHandleNewObj = {}
  const amList = [] // 单纯加减数量的列表
  const sameList = [] // 完全相同的列表
  const sameKeys = [] // 编号、规格、长度相同的列表
  const oldObj = arr2obj(oldList, 'serialNumber')

  for (let i = 0; i < newList.length; i++) {
    const n = newList[i]
    const _o = oldObj[n.serialNumber]
    if (n.changeTypeEnum) {
      n.operateType = n.changeTypeEnum & changeTypeEnum.EDIT.V ? assembleOperateTypeEnum.EDIT.V : assembleOperateTypeEnum.NEW.V
    }
    if (n?.changeAssembleLinkList?.length) {
      n.handleObj = {}
      n.oldSerialNumbers = []
      for (const item of n.changeAssembleLinkList) {
        n.handleObj[item.oldAssembleSerialNumber] = {
          ...item,
          handleType: item.assembleChangeTypeEnum
        }
        n.oldSerialNumbers.push(item.oldAssembleSerialNumber)
      }
    }
    if (isNotBlank(_o)) {
      sameKeys.push(`${n.serialNumber}`)
      // if (isNotBlank(_o) && _o.specification === n.specification && _o.length === n.length) {
      //   sameKeys.push(`${n.serialNumber}_${n.specification}_${n.length}`)
      if (_o.quantity === n.quantity) {
        sameList.push({ newAssemble: n, oldAssemble: _o })
        continue
      } else {
        const diffQuantity = _o.quantity - n.quantity
        const changeType = n.diffQuantity > 0 ? changeTypeEnum.REDUCE.V : changeTypeEnum.ADD.V
        const diffTotalWeight = toPrecision(_o.totalNetWeight - n.totalNetWeight, 2)
        amList.push({ newAssemble: n, oldAssemble: _o, changeType, diffQuantity, diffTotalWeight })
      }
    } else {
      // 默认为新增
      if (!n.operateType) n.operateType = assembleOperateTypeEnum.NEW.V
      needHandleNewList.push(n)
      needHandleNewObj[n.serialNumber] = n
    }
  }

  for (let i = 0; i < oldList.length; i++) {
    const o = oldList[i]
    // const _key = `${o.serialNumber}_${o.specification}_${o.length}`
    const _key = `${o.serialNumber}`
    if (sameKeys.includes(_key)) {
      continue
    } else {
      needHandleOldList.push(o)
      needHandleOldObj[o.serialNumber] = o
    }
  }

  return {
    needHandleOldList,
    needHandleNewList,
    needHandleOldObj,
    needHandleNewObj,
    amList,
    sameList
  }
}

// 处理比较变更部件
function handleCompareAssembleList(assembleInfo) {
  let isAssembleHandled = true
  const assembleChangeList = []
  const oldHandledSNs = []
  const assembleDelList = []
  for (let i = 0; i < assembleInfo.needHandleNewList.length; i++) {
    const v = assembleInfo.needHandleNewList[i]
    if (!v.operateType || (v.operateType !== assembleOperateTypeEnum.NEW.V && !v.oldSerialNumbers?.length)) isAssembleHandled = false

    if (v.operateType === assembleOperateTypeEnum.NEW.V) {
      const changeType = changeTypeEnum.NEW.V
      const diffQuantity = -v.quantity
      const diffTotalWeight = -v.totalNetWeight
      assembleChangeList.push({ newAssemble: v, oldAssemble: {}, changeType, diffQuantity, diffTotalWeight })
    }

    if (v.operateType !== assembleOperateTypeEnum.NEW.V && v.oldSerialNumbers?.length) {
      for (let o = 0; o < v.oldSerialNumbers.length; o++) {
        const oldSN = v.oldSerialNumbers[o]
        oldHandledSNs.push(oldSN)
        if (!v.handleObj[oldSN] || !v.handleObj[oldSN]?.handleType || !v.handleObj[oldSN]?.quantity) {
          isAssembleHandled = false
          continue
        }

        const changeType = changeTypeEnum.EDIT.V
        const diffQuantity = v.handleObj[oldSN]?.quantity - v.quantity
        const diffTotalWeight = toPrecision(v.handleObj[oldSN]?.quantity * v.handleObj[oldSN]?.netWeight - v.totalNetWeight, 2)
        assembleChangeList.push({
          newAssemble: v,
          oldAssemble: assembleInfo?.needHandleOldObj?.[oldSN],
          oldQuantity: v.handleObj[oldSN]?.quantity,
          handleType: v.handleObj[oldSN]?.handleType,
          changeType,
          diffQuantity,
          diffTotalWeight
        })
      }
    }
  }
  if (isAssembleHandled && oldHandledSNs.length !== assembleInfo.needHandleOldList.length) {
    for (let i = 0; i < assembleInfo.needHandleOldList.length; i++) {
      const o = assembleInfo.needHandleOldList[i]
      if (!oldHandledSNs.includes(o.serialNumber)) {
        const changeType = changeTypeEnum.DEL.V
        const diffQuantity = o.quantity
        const diffTotalWeight = o.totalNetWeight
        assembleDelList.push({ newAssemble: {}, oldAssemble: o, changeType, diffQuantity, diffTotalWeight })
      }
    }
  }

  return {
    assembleCompareList: [...assembleChangeList, ...assembleInfo?.amList, ...assembleDelList],
    isAssembleHandled
  }
}

// 处理比较零件变更
function handleComparePartList(oldList, newList) {
  const resObj = {}
  const oldObj = arr2obj(oldList, 'serialNumber')
  const newObj = {}
  for (let i = 0; i < newList.length; i++) {
    const n = newList[i]
    const _o = oldObj[n.serialNumber]
    newObj[n.serialNumber] = n
    const newQuantity = n.quantity
    let changeType, diffQuantity, diffTotalWeight, oldQuantity
    // 新增
    if (!_o) {
      changeType = changeTypeEnum.NEW.V
      oldQuantity = 0
      diffQuantity = -newQuantity
      diffTotalWeight = -n.totalNetWeight
    } else if (_o.quantity === n.quantity) {
      continue
    } else {
      oldQuantity = _o.quantity
      diffQuantity = _o.quantity - newQuantity
      diffTotalWeight = toPrecision(_o.totalNetWeight - n.totalNetWeight, 2)
      changeType = n.diffQuantity > 0 ? changeTypeEnum.REDUCE.V : changeTypeEnum.ADD.V
    }
    resObj[n.serialNumber] = { ...n, changeType, diffQuantity, diffTotalWeight, oldQuantity, newQuantity }
  }
  for (let i = 0; i < oldList.length; i++) {
    const o = oldList[i]
    const oldQuantity = o.quantity
    let changeType, diffQuantity, diffTotalWeight, newQuantity
    if (!newObj[o.serialNumber]) {
      changeType = changeTypeEnum.DEL.V
      diffQuantity = oldQuantity
      diffTotalWeight = o.totalNetWeight
      newQuantity = 0
    } else {
      continue
    }
    resObj[o.serialNumber] = { ...o, oldQuantity, changeType, diffQuantity, diffTotalWeight, newQuantity }
  }
  return obj2arr(resObj)
}
