<template>
  <el-tree
    ref="treeMenuRef"
    v-loading="loading"
    :data="formatTreeDate"
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
      <div style="padding: 3px 5px; border-radius: 3px; width: 100%; position: relative" class="tree-custom-content">
        <template v-if="data.rowKey.indexOf('project') === -1">
          <div style="position: absolute; width: 10px; border-bottom: 1px dashed #dcdfe6; height: 1px; top: 50%; left: -10px"></div>
          <div
            style="position: absolute; width: 1px; border-right: 1px dashed #dcdfe6; left: -10px"
            :style="{
              height: data.isLast ? 'calc(50% - 2px)' : data.children?.length && node.expanded ? `${1 + data.children?.length}00%` : '100%',
            }"
          ></div>
        </template>
        <div style="width: 100%; overflow: hidden; text-overflow: ellipsis">
          <svg-icon style="margin-right: 5px" :icon-class="data.icon" />
          <el-tooltip effect="dark" :content="node.label || ''" placement="top-start">
            <span :style="`font-size:${data.fontSize}px;${node.isLeaf ? '' : `font-weight: bold;`}`">{{ node.label }}</span>
          </el-tooltip>
        </div>
        <!-- <span style="float: right; padding: 0 2px 0 6px; font-size: 10px; color: #ccc">
            <span>{{ data.type }}</span>
          </span> -->
      </div>
    </template>
  </el-tree>
</template>

<script setup>
import { defineProps, ref, watch, watchEffect, defineEmits } from 'vue'
import { projectNameFormatter } from '@/utils/project'

const emit = defineEmits(['nesting-task-click'])

const props = defineProps({
  filterText: {
    type: String
  },
  loading: {
    type: Boolean,
    default: false
  },
  treeData: {
    type: Array,
    default: () => []
  }
})

const treeMenuRef = ref()
const filterIds = ref([])
const expandedKeys = ref([])
// const currentMonomerId = ref()
const formatTreeDate = ref([])

watchEffect(() => {
  formatTreeDate.value = dataFormat(props.treeData)
})

function dataFormat(content) {
  const _tree = []
  expandedKeys.value = []
  for (let i = 0; i < content.length; i++) {
    const monomers = content[i].children
    const _monomer = []
    for (let x = 0; x < monomers.length; x++) {
      const areas = monomers[x].children
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
          projectId: content[i].id,
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
        disabled: false,
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
      disabled: false,
      type: '项目',
      icon: 'project'
    })
  }
  return _tree
}

// 切换区域
// function handleNodeClick(data, node) {
//   if (data.isLeaf) {
//     if (!node.checked) {
//       // 区域多选
//       // if (data.monomerId === currentMonomerId.value) {
//       //   treeMenuRef.value.setChecked(node, true)
//       // } else {
//       //   currentMonomerId.value = data.monomerId
//       //   treeMenuRef.value.setCheckedNodes([])
//       //   treeMenuRef.value.setChecked(node, true)
//       // }
//       // 区域单选
//       treeMenuRef.value.setCheckedNodes([])
//       treeMenuRef.value.setChecked(node, true)
//     } else {
//       treeMenuRef.value.setChecked(node, false)
//     }

//     emit('nesting-task-click', treeMenuRef.value.getCheckedNodes(true))
//   }
// }

// 切换区域
function handleCheckClick(data, node) {
  const _keys = treeMenuRef.value.getCheckedKeys()
  const areaIds = []
  const monomerIds = []
  const projectIds = []
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
  })
  emit('nesting-task-click', { areaIds, monomerIds, projectIds })
}

// tree过滤输入监听
watch(
  () => props.filterText,
  (val) => {
    filterIds.value = []
    treeMenuRef.value.filter(val)
  }
)

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

  // ::v-deep(.el-tree-node.is-checked > .el-tree-node__content .tree-custom-content) {
  //   background-color: #ffe48d !important;
  // }
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
