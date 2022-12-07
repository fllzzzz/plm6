<template>
  <div class="head-container">
    <project-header-time v-model="month" :data="timeList" @change="fetchTree" empty-text="暂无构件排产信息" />
    <!-- <el-date-picker
      v-model="month"
      type="month"
      size="small"
      value-format="x"
      :clearable="false"
      placeholder="查询月份"
      style="width: 38%"
      :disabled-date="disabledDate"
      class="filter-item"
      @change="fetchTree"
    />
    <el-input
      v-model.trim="filterText"
      class="filter-item"
      size="small"
      style="width: 58%"
      clearable
      placeholder="输入项目/单体/区域搜索"
    /> -->
  </div>
  <div :style="heightStyle">
    <el-tree
      ref="treeMenuRef"
      v-loading="loading"
      :data="treeData"
      :props="{ children: 'children', label: 'label' }"
      :filter-node-method="filterNode"
      style="height: 100%"
      :indent="20"
      expand-on-click-node
      node-key="rowKey"
      :auto-expand-parent="false"
      :default-expanded-keys="expandedKeys"
      @node-click="handleNodeClick"
    >
      <template #default="{ node, data }">
        <div style="padding: 3px 5px; border-radius: 3px; width: 100%; position: relative" class="tree-custom-content">
          <template v-if="data.rowKey.indexOf('project') === -1">
            <div style="position: absolute; width: 10px; border-bottom: 1px dashed #dcdfe6; height: 1px; top: 50%; left: -10px"></div>
            <div
              style="position: absolute; width: 1px; border-right: 1px dashed #dcdfe6; left: -10px"
              :style="{
                height: data.isLast
                  ? 'calc(50% - 2px)'
                  : data.children?.length && node.expanded
                  ? `${1 + data.children?.length}00%`
                  : '100%',
              }"
            ></div>
          </template>
          <div style="width: 100%; overflow: hidden; text-overflow: ellipsis">
            <svg-icon style="margin-right: 5px" :icon-class="data.icon" />
            <span :style="`font-size:${data.fontSize}px;${node.isLeaf ? '' : `font-weight: bold;`}`">{{ node.label }}</span>
          </div>
          <!-- <span style="float: right; padding: 0 2px 0 6px; font-size: 10px; color: #ccc">
            <span>{{ data.type }}</span>
          </span> -->
        </div>
      </template>
    </el-tree>
  </div>
</template>

<script setup>
import { getProjectToAreaTree, getAreaTreeTime } from '@/api/mes/scheduling-manage/artifact'
import { ref, defineProps, defineEmits, watch } from 'vue'
import moment from 'moment'

import { projectNameFormatter } from '@/utils/project'
import checkPermission from '@/utils/system/check-permission'
import { artifactSchedulingPM as permission } from '@/page-permission/mes'

import projectHeaderTime from '@/views/mes/scheduling-manage/common/project-header-time.vue'

const emit = defineEmits(['area-click'])
defineProps({
  heightStyle: {
    type: String,
    default: ''
  }
})

const treeMenuRef = ref()
const currentMonomerId = ref()

const curMonthValue = moment().startOf('month').valueOf()

const timeList = ref([])
const month = ref()
const filterText = ref()
const filterIds = ref([])
const treeData = ref([])
const loading = ref(false)
const expandedKeys = ref([])

fetchTime()

async function fetchTime() {
  if (!checkPermission(permission.get)) return
  try {
    const { content } = await getAreaTreeTime()
    const timeStamp = []
    timeList.value = content.map((v) => {
      const _t = moment(v, 'YYYY/MM')
      timeStamp.push(_t.valueOf())
      return {
        year: _t.year(),
        month: _t.month() + 1,
        timeStamp: _t.valueOf()
      }
    })
    // timeList.value = timeList.value.concat(timeList.value).concat(timeList.value).concat(timeList.value)
    if (timeStamp?.length) {
      month.value = timeStamp.includes(curMonthValue) ? curMonthValue : timeStamp[0]
      fetchTree()
    }
  } catch (error) {
    console.log('获取排程信息时间错误', error)
  }
}

// function disabledDate(time) {
//   return timeList.value?.indexOf(moment(time).valueOf()) === -1
// }

async function fetchTree() {
  try {
    loading.value = true
    expandedKeys.value = []
    const { content } = await getProjectToAreaTree({
      dateTime: month.value
    })
    treeData.value = await dataFormat(content)
  } catch (error) {
    console.log('获取排程信息，项目树错误', error)
  } finally {
    loading.value = false
  }
}

function dataFormat(content) {
  const _tree = []
  for (let i = 0; i < content.length; i++) {
    const monomers = content[i].monomerSchedulingDetailDO
    const _monomer = []
    for (let x = 0; x < monomers.length; x++) {
      const areas = monomers[x].areaSchedulingDetailDO
      const _area = []
      for (let y = 0; y < areas.length; y++) {
        const rowKey = 'area_' + areas[y].id
        _area.push({
          id: areas[y].id,
          isLast: y === areas.length - 1,
          rowKey: rowKey,
          label: areas[y].name,
          name: areas[y].name,
          parentIds: [monomers[x].id, content[i].id],
          monomerId: monomers[x].id,
          factoryId: areas[y].factory?.id,
          workshopId: areas[y].workshop?.id,
          endDate: areas[y].endDate,
          isLeaf: true,
          disabled: false,
          fontSize: 14,
          type: '',
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
        disabled: true,
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
      disabled: true,
      type: '项目',
      icon: 'project'
    })
  }
  return _tree
}

// 切换区域
function handleNodeClick(data, node) {
  if (data.isLeaf) {
    // console.log(data, node, currentMonomerId.value)
    if (!node.checked) {
      if (data.monomerId === currentMonomerId.value) {
        treeMenuRef.value.setChecked(node, true)
      } else {
        currentMonomerId.value = data.monomerId
        treeMenuRef.value.setCheckedNodes([])
        treeMenuRef.value.setChecked(node, true)
      }
    } else {
      treeMenuRef.value.setChecked(node, false)
    }

    emit('area-click', treeMenuRef.value.getCheckedNodes(true))
  }
}

// tree过滤输入监听
watch(filterText, (val) => {
  filterIds.value = []
  treeMenuRef.value.filter(val)
})

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
</script>

<style lang="scss" scoped>
.el-tree {
  // width: 360px;
  overflow-y: auto;
  padding-right: 5px;
  font-size: 15px;

  ::v-deep(.el-tree-node.is-checked > .el-tree-node__content .tree-custom-content) {
    background-color: #ffe48d !important;
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
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
