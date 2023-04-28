import { watch, ref, watchEffect } from 'vue'
import { enclosureGroupsTree } from '@/api/common'

// 获取围护的生产班组信息：工厂-车间-生产线-生产班组
const useSchedulingGroups = ({ queryParams, factoryIds = [], disabledIds }) => {
  const loaded = ref(false)
  const options = ref([])
  const originOptions = ref([])
  const groupsObj = ref({})

  watch(
    () => queryParams.value,
    () => {
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
    if (!queryParams.value?.type) return
    const content = await enclosureGroupsTree(queryParams.value)
    originOptions.value = content
    loaded.value = true
    initOptions(originOptions.value)
  }

  function initOptions(list) {
    options.value = []
    groupsObj.value = {}
    if (loaded.value) {
      const { list: _groupsTree, obj: _groupsObj } = dataFormat(list, factoryIds.value, [], true)
      options.value = _groupsTree
      groupsObj.value = _groupsObj
    }
  }

  watchEffect(() => {
    setData(options.value, disabledIds?.value)
  })

  function setData(list, disabledIds = []) {
    for (let i = 0; i < list.length; i++) {
      if (list[i]?.children?.length) {
        setData(list[i].children, disabledIds)
      } else {
        list[i].disabled = disabledIds && disabledIds?.includes(list[i].id) || false
      }
    }
  }

  function getCurGroupsTree(workshopId) {
    return workshopId ? options.value.filter(v => v.id === workshopId) : options.value
  }

  return {
    getCurGroupsTree,
    groupsTree: options,
    groupsObj
  }
}

function dataFormat(list, _factoryIds, disabledIds = [], returnAll = false) {
  if (_factoryIds?.length || returnAll) {
    const _groupsObj = {}
    const _factory = []
    for (let f = 0; f < list.length; f++) {
      const _workshops = []
      const workshops = list[f].workshopList
      for (let w = 0; w < workshops.length; w++) {
        const _lines = []
        const productionLines = workshops[w].productionLineList
        for (let p = 0; p < productionLines.length; p++) {
          const _groups = []
          const groups = productionLines[p].teamList
          for (let g = 0; g < groups.length; g++) {
            let leaderName = ''
            groups[g].userLinkList.forEach(m => {
              if (m.boolLeaderEnum) {
                leaderName = m.userName
              }
            })
            _groups.push({
              id: groups[g].id,
              name: leaderName + ' - ' + groups[g].processName,
              disabled: disabledIds.includes(groups[g].id)
            })
            _groupsObj[groups[g].id] = {
              productionLine: { id: productionLines[p].id, name: productionLines[p].name },
              factory: { id: list[f].id, name: list[f].name },
              workshop: { id: workshops[w].id, name: workshops[w].name },
              groupsName: leaderName + ' - ' + groups[g].processName
            }
          }
          _lines.push({
            id: productionLines[p].id,
            name: productionLines[p].name,
            children: _groups
          })
        }
        _workshops.push({
          id: workshops[w].id,
          name: workshops[w].name,
          children: _lines
        })
      }
      _factory.push({
        id: list[f].id,
        name: list[f].name,
        children: _workshops
      })
    }
    return {
      list: _factory,
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
