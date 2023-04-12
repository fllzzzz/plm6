import { watch, ref, watchEffect } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import { getGroupsTree } from '@/api/mes/scheduling-manage/artifact'

// 获取排产使用的生产班组信息：工厂-车间-生产线-生产班组
const useSchedulingGroups = ({ queryParams, factoryIds, disabledIds }) => {
  const loaded = ref(false)
  const options = ref([])
  const originOptions = ref([])
  const groupsObj = ref({})

  watch(
    () => queryParams.value,
    () => {
      // console.log('useSchedulingGroups', queryParams.value, factoryIds)
      loaded.value = false
      fetchGroupsTree()
    },
    { immediate: true }
  )

  watch(
    () => factoryIds,
    () => {
      initOptions(originOptions.value)
    },
    { deep: true }
  )

  async function fetchGroupsTree() {
    if (!queryParams.value?.productType || !queryParams.value?.structureClassId) return
    const content = await getGroupsTree(queryParams.value)
    originOptions.value = content
    loaded.value = true
    initOptions(originOptions.value)
  }

  function initOptions(list) {
    options.value = []
    groupsObj.value = {}
    if (loaded.value) {
      const { list: _groupsTree, obj: _groupsObj } = dataFormat(list, factoryIds.value)
      // console.log(_groupsObj, 'initOptions')
      options.value = _groupsTree
      groupsObj.value = _groupsObj
    }
  }

  watchEffect(() => {
    setData(options.value, disabledIds?.value)
  })

  function setData(list, disabledIds = []) {
    for (let i = 0; i < list.length; i++) {
      // console.log(list[i], disabledIds, 'disabledIds')
      if (list[i]?.children?.length) {
        setData(list[i].children, disabledIds)
      } else {
        list[i].disabled = disabledIds && disabledIds?.includes(list[i].id) || false
      }
    }
  }

  function getCurGroupsTree(workshopId) {
    // console.log('getCurGroupsTree', options.value)
    return workshopId ? options.value.filter(v => v.id === workshopId) : options.value
  }

  return {
    getCurGroupsTree,
    groupsTree: options,
    groupsObj
  }
}

// 手动获取
export async function manualFetchGroupsTree({ productType, structureClassId, _factoryIds, disabledIds = [] }, returnAll = false) {
  // 零件不必传structureClassId
  if ((!productType || (!structureClassId && productType !== componentTypeEnum.MACHINE_PART.V)) && !returnAll) {
    return {
      list: [],
      obj: {}
    }
  }
  const content = await getGroupsTree({ productType, structureClassId })
  // 零件返回全部
  const _returnAll = Boolean(productType === componentTypeEnum.MACHINE_PART.V) || returnAll
  return dataFormat(content, _factoryIds, disabledIds, _returnAll)
}

function dataFormat(list, _factoryIds, disabledIds = [], returnAll = false) {
  if (_factoryIds?.length || returnAll) {
    let _allWorkshops = []
    const _groupsObj = {}
    for (let i = 0; i < list.length; i++) {
      if (_factoryIds?.indexOf(list[i].id) !== -1 || returnAll) {
        _allWorkshops = _allWorkshops.concat(list[i].workshopList)
      }
    }
    const _workshops = []
    // console.log({ _allWorkshops })
    const workshops = _allWorkshops
    for (let w = 0; w < workshops.length; w++) {
      const _lines = []
      const productionLines = workshops[w].productionLineList
      for (let p = 0; p < productionLines.length; p++) {
        const _groups = []
        const groups = productionLines[p].groupsList
        for (let g = 0; g < groups.length; g++) {
          _groups.push({
            id: groups[g].id,
            name: groups[g].name,
            disabled: disabledIds.includes(groups[g].id)
          })
          _groupsObj[groups[g].id] = {
            productionLine: { id: productionLines[p].id, name: productionLines[p].name },
            workshop: { id: workshops[w].id, name: workshops[w].name },
            groupsName: groups[g].name
          }
        }
        _lines.push({
          id: productionLines[p].id,
          name: productionLines[p].name,
          children: _groups
        })
      }
      // console.log({ workshops: workshops[w] })
      _workshops.push({
        id: workshops[w].id,
        name: workshops[w].name,
        children: _lines
      })
      // console.log({ _workshops })
    }
    return {
      list: _workshops,
      obj: _groupsObj
    }
  } else {
    return {
      list: [],
      obj: {}
    }
  }
}

export default useSchedulingGroups
