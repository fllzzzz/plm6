import { ref, provide } from 'vue'
import { arr2obj, obj2arr } from '@/utils/convert/type'
import { toPrecision, deepClone } from '@/utils/data-type'

export default function useHandelSummaryData() {
  const summaryInfo = ref({})
  provide('summaryInfo', summaryInfo)

  // 处理汇总数据
  const handleSummaryData = (changeInfo) => {
    const artifactObj = {}
    const assembleObj = {}
    const partObj = {}
    for (const artifact of changeInfo) {
      const _old = deepClone(artifact.oldArtifact)
      const _new = deepClone(artifact.newArtifact)
      const _key = `${_old.serialNumber}_${_old.specification}_${_old.length}_${_old.netWeight}__${_new.serialNumber}_${_new.specification}_${_new.length}_${_new.netWeight}`
      const diffQuantity = _old.quantity - _new.quantity || 0
      const diffTotalWeight = toPrecision(_old.totalNetWeight - _new.totalNetWeight || 0, 2)
      if (!artifactObj[_key]) {
        artifactObj[_key] = {
          oldArtifact: _old,
          newArtifact: _new,
          processSummary: arr2obj(_old.processSummaryList || [], 'name'),
          diffLength: _old.length - _new.length || 0,
          diffQuantity,
          diffTotalWeight
        }
      } else {
        artifactObj[_key].oldArtifact.quantity += _old.quantity
        artifactObj[_key].newArtifact.quantity += _new.quantity
        artifactObj[_key].diffQuantity += diffQuantity
        artifactObj[_key].diffTotalWeight += diffTotalWeight
        mergeProcessSummary(artifactObj[_key].processSummary, _old.processSummaryList)
      }

      if (artifact?.assembleCompareList?.length) {
        for (const assemble of artifact.assembleCompareList) {
          const _old = assemble.oldAssemble ? deepClone(assemble.oldAssemble) : {}
          const _new = assemble.newAssemble ? deepClone(assemble.newAssemble) : {}
          const _key = `${_old.serialNumber}_${_old.specification}_${_old.length}_${_old.netWeight}__${_new.serialNumber}_${_new.specification}_${_new.length}_${_new.netWeight}`
          if (!assembleObj[_key]) {
            assembleObj[_key] = {
              ...deepClone(assemble),
              processSummary: arr2obj(_old.processSummaryList || [], 'name')
            }
          } else {
            assembleObj[_key].oldAssemble.quantity += _old.quantity
            assembleObj[_key].newAssemble.quantity += _new.quantity
            assembleObj[_key].diffQuantity += assemble.diffQuantity
            assembleObj[_key].diffTotalWeight += assemble.diffTotalWeight
            mergeProcessSummary(assembleObj[_key].processSummary, _old.processSummaryList)
          }
        }
      }

      if (artifact?.partCompareList?.length) {
        for (const part of artifact.partCompareList) {
          const _key = `${part.serialNumber}_${part.specification}_${part.length}_${part.netWeight}_${part.changeType}`
          if (!partObj[_key]) {
            partObj[_key] = {
              ...deepClone(part),
              processSummary: arr2obj(part.processSummaryList || [], 'name')
            }
          } else {
            partObj[_key].oldQuantity += part.oldQuantity
            partObj[_key].newQuantity += part.newQuantity
            partObj[_key].diffQuantity += part.diffQuantity
            partObj[_key].diffTotalWeight += part.diffTotalWeight
            mergeProcessSummary(partObj[_key].processSummary, part.processSummaryList)
          }
        }
      }
    }

    summaryInfo.value = {
      artifactList: obj2arr(artifactObj),
      assembleList: obj2arr(assembleObj),
      partList: obj2arr(partObj)
    }
  }

  return {
    handleSummaryData,
    summaryInfo
  }
}

function mergeProcessSummary(obj, needList) {
  if (!needList) return
  for (const item of needList) {
    if (!obj[item.name]) {
      obj[item.name] = item
    } else {
      obj[item.name].quantity += item.quantity
    }
  }
}
