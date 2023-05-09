<template>
  <div class="head-container">
    <!-- <el-date-picker
      v-model="month"
      type="month"
      size="small"
      value-format="x"
      format="YYYY年MM月"
      :clearable="false"
      placeholder="选择月份"
      style="width: 48%"
      class="filter-item"
      @change="fetchTime(null)"
    /> -->
    <project-header-time v-model="month" multiple :data="timeList" @change="fetchProject" empty-text="暂无零件排产信息" />
    <!-- <common-select
      v-loading="timeLoading"
      v-model="month"
      :options="timeList"
      :loading="timeLoading"
      loading-text="加载中"
      clearable
      multiple
      default
      style="width: 48%"
      class="filter-item"
      :placeholder="timeLoading ? '加载中' : '选择月份'"
      :dataStructure="{ key: 'timeStamp', label: 'month', value: 'timeStamp' }"
      @change="fetchProject"
    /> -->
  </div>
  <div :style="`height:${maxHeight - 30}px`">
    <el-tree
      ref="treeMenuRef"
      v-loading="projectLoading"
      :data="treeData"
      :props="{ children: 'children', label: 'label' }"
      :filter-node-method="filterNode"
      style="height: 100%"
      :indent="20"
      show-checkbox
      expand-on-click-node
      node-key="rowKey"
      :auto-expand-parent="false"
      :default-expanded-keys="expandedKeys"
      @check-change="handleCheckClick"
    >
      <template #default="{ node, data }">
        <div style="padding: 3px; border-radius: 3px; width: 100%; position: relative">
          <div style="width: 100%; overflow: hidden; text-overflow: ellipsis">
            <svg-icon style="margin-right: 5px" :icon-class="data.icon" />
            <span :style="`font-size:${data.fontSize}px;${node.isLeaf ? '' : `font-weight: bold;`}`">{{ node.label }}</span>
          </div>
        </div>
      </template>
    </el-tree>
  </div>
</template>

<script setup>
import { getInfo } from '@/api/config/mes/base'
import { getProject, getMonth } from '@/api/mes/scheduling-manage/machine-part'
import { ref, defineProps, defineEmits, defineExpose } from 'vue'
import { isBlank, isNotBlank } from '@/utils/data-type'
import moment from 'moment'

import { projectNameFormatter } from '@/utils/project'
import checkPermission from '@/utils/system/check-permission'
import { machinePartSchedulingPM as permission } from '@/page-permission/mes'

import projectHeaderTime from '@/views/mes/scheduling-manage/common/project-header-time.vue'

const emit = defineEmits(['project-click'])
defineProps({
  maxHeight: {
    type: [Number, String],
    default: undefined
  }
})

const timeList = ref([])
const timeLoading = ref(false)
const treeMenuRef = ref()
const month = ref([])
const treeData = ref([])
const projectLoading = ref(false)
const filterIds = ref([])
const expandedKeys = ref([])
const configData = ref()

fetchTime()
fetchConfig()

async function fetchConfig() {
  try {
    const data = await getInfo()
    configData.value = data
  } catch (error) {
    console.log('获取车间、产线、生产组的配置层级失败', error)
  }
}

async function fetchTime(lastQuery) {
  if (!checkPermission(permission.get)) return
  try {
    timeList.value = []
    treeData.value = []
    month.value = []
    timeLoading.value = true
    const { content } = await getMonth()
    timeList.value = content.map((v) => {
      const timeStamp = moment(v, 'YYYY-MM').valueOf()
      const _arr = v.split('-')
      return {
        timeStamp,
        year: _arr[0],
        month: _arr[1]
      }
    })
    // if (timeList.value?.length) {
    //   date.value = timeList.value[0].dateTime
    //   fetchProject()
    // }
    if (lastQuery && isNotBlank(lastQuery.monthList) && timeList.value?.length) {
      for (let i = 0; i < lastQuery.monthList.length; i++) {
        const m = lastQuery.monthList[i]
        if (timeList.value?.findIndex((v) => v.timeStamp === m) !== -1) {
          month.value.push(m)
        }
      }
    }
    if (isBlank(month.value) && timeList.value?.length) {
      const curTime = moment().startOf('month').valueOf()
      const curIndex = timeList.value?.findIndex((v) => v.timeStamp === curTime)
      if (curIndex !== -1) {
        month.value.push(curTime)
      } else {
        month.value = [timeList.value[0].timeStamp]
      }
    }
    await fetchProject(lastQuery)
  } catch (error) {
    console.log('获取排程信息时间错误', error)
  } finally {
    timeLoading.value = false
  }
}

// function disabledDate(time) {
//   return timeList.value?.indexOf(moment(time).valueOf()) === -1 && moment(time).month() === month.value
// }

async function fetchProject(lastQuery) {
  treeData.value = []
  if (!checkPermission(permission.get) || isBlank(month.value)) return
  try {
    projectLoading.value = true
    const { content } = await getProject({
      // dateTime: date.value,
      monthList: month.value
    })
    treeData.value = await dataFormat(content)
  } catch (error) {
    console.log('获取排程信息，项目树错误', error)
  } finally {
    projectLoading.value = false
  }
}

function dataFormat(content) {
  const _tree = []
  for (let i = 0; i < content.length; i++) {
    const monomers = content[i].monomerList
    const _monomer = []
    for (let x = 0; x < monomers.length; x++) {
      const areas = monomers[x].areaList
      const _area = []
      for (let y = 0; y < areas.length; y++) {
        const workshops = areas[y].workshopList
        const productionLines = areas[y].productionLineList
        const groups = areas[y].groupsList
        const _workshop = []
        const _productionLine = []
        const _groups = []
        for (let w = 0; w < workshops?.length; w++) {
          const rowKey = 'workshop_' + workshops[w].id
          _workshop.push({
            id: workshops[w].id,
            isLast: w === workshops.length - 1,
            rowKey: rowKey,
            label: workshops[w].name,
            name: workshops[w].name,
            parentIds: [areas[y].id, monomers[x].id, content[i].id],
            areaId: areas[y].id,
            isLeaf: true,
            fontSize: 14,
            type: '',
            icon: 'config-2'
          })
          expandedKeys.value.push(rowKey)
        }
        for (let p = 0; p < productionLines?.length; p++) {
          const rowKey = 'productionLine_' + productionLines[p].id
          _productionLine.push({
            id: productionLines[p].id,
            isLast: p === productionLines.length - 1,
            rowKey: rowKey,
            label: productionLines[p].name,
            name: productionLines[p].name,
            parentIds: [areas[y].id, monomers[x].id, content[i].id],
            areaId: areas[y].id,
            isLeaf: true,
            fontSize: 14,
            type: '',
            icon: 'config-2'
          })
          expandedKeys.value.push(rowKey)
        }
        for (let g = 0; g < groups?.length; g++) {
          const rowKey = 'groups_' + groups[g].id
          _groups.push({
            id: groups[g].id,
            isLast: g === groups.length - 1,
            rowKey: rowKey,
            label: groups[g].name,
            name: groups[g].name,
            parentIds: [areas[y].id, monomers[x].id, content[i].id],
            areaId: areas[y].id,
            isLeaf: true,
            fontSize: 14,
            type: '',
            icon: 'config-2'
          })
          expandedKeys.value.push(rowKey)
        }
        const rowKey = 'area_' + areas[y].id
        _area.push({
          id: areas[y].id,
          isLast: y === areas.length - 1,
          rowKey: rowKey,
          parentIds: [monomers[x].id, content[i].id],
          label: areas[y].name,
          children: workshops?.length ? _workshop : productionLines?.length ? _productionLine : _groups,
          isLeaf: true,
          fontSize: 15,
          type: '区域',
          icon: 'config-2'
        })
        expandedKeys.value.push(rowKey)
      }
      const rowKey = 'monomer_' + monomers[x].id
      _monomer.push({
        id: monomers[x].id,
        isLast: x === monomers.length - 1,
        rowKey: rowKey,
        parentIds: [content[i].id],
        label: monomers[x].name,
        children: _area,
        isLeaf: false,
        fontSize: 15,
        type: '单体',
        icon: 'document'
      })
      expandedKeys.value.push(rowKey)
    }
    _tree.push({
      id: content[i].id,
      isLast: i === content.length - 1,
      rowKey: 'project_' + content[i].id,
      parentIds: [],
      label: projectNameFormatter(
        {
          serialNumber: content[i].serialNumber,
          name: content[i].name,
          shortName: content[i].shortName
        },
        { showProjectFullName: false, showSerialNumber: true },
        false
      ),
      children: _monomer,
      isLeaf: false,
      fontSize: 16,
      type: '项目',
      icon: 'project'
    })
  }
  return _tree
}

// 过滤数据
function filterNode(value, data, node) {
  if (!value) return true
  if (data.label.includes(value) || judgeFilterIds(data.parentIds)) {
    filterIds.value.push(data.id)
    return true
  }
}

// 返回所有子节点
function judgeFilterIds(ids) {
  let flag = false
  for (let i = 0; i < ids.length; i++) {
    if (filterIds.value.includes(ids[i])) {
      flag = true
    }
  }
  return flag
}

// 切换区域
function handleCheckClick(data, node) {
  const _keys = treeMenuRef.value.getCheckedKeys()
  const areaIds = []
  const monomerIds = []
  const projectIds = []
  const workshopIds = []
  const productionLineIds = []
  const groupsIds = []
  _keys.forEach((v) => {
    const _id = v.split('_')[1]
    if (v.indexOf('project') !== -1) {
      projectIds.push(_id)
    }
    if (v.indexOf('monomer') !== -1) {
      monomerIds.push(_id)
    }
    if (v.indexOf('area') !== -1) {
      areaIds.push(_id)
    }
    if (v.indexOf('workshop') !== -1) {
      workshopIds.push(_id)
    }
    if (v.indexOf('productionLine') !== -1) {
      productionLineIds.push(_id)
    }
    if (v.indexOf('groups') !== -1) {
      groupsIds.push(_id)
    }
  })
  emit('project-click', { areaIds, monomerIds, projectIds, workshopIds, productionLineIds, groupsIds }, month.value, configData.value)
}

defineExpose({
  refresh: fetchTime
})
</script>

<style lang="scss" scoped>
.el-tree {
  // width: 360px;
  overflow-y: auto;
  padding-right: 5px;
  font-size: 15px;

  // ::v-deep(.el-tree-node.is-checked > .el-tree-node__content .tree-custom-content) {
  //   background-color: #ffe48d !important;
  // }
  ::v-deep(.el-tree-node__content > label.el-checkbox) {
    margin-right: 3px;
  }
  ::v-deep(.el-tree-node__content:hover) {
    background-color: transparent !important;
  }

  ::v-deep(.el-tree-node:focus > .el-tree-node__content) {
    background-color: transparent !important;
  }

  ::v-deep(.el-tree-node__content > .el-tree-node__expand-icon) {
    display: none;
  }

  ::v-deep(.el-checkbox__inner) {
    border: 2px solid #000;
  }
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
