<template>
  <div>
    <el-card :style="{ height: `${maxHeight}px` }">
      <template #header>
        <div class="card-header flex-css">
          <div class="flex-rbc">
            <div class="flex-rsc">
              <span>{{ title }}</span>
              <el-tag v-show="labelTip" size="small" style="margin-left:5px">{{ labelTip }}</el-tag>
            </div>
            <div>
              <common-button v-permission="permission.add" size="small" type="success" icon="el-icon-plus" @click="add()" />
              <common-button
                v-permission="permission.del"
                size="small"
                :type="selected.delBtn ? 'danger' : undefined"
                icon="el-icon-delete"
                @click="del()"
              />
            </div>
          </div>
          <div class="flex-rbc">
            <el-input v-model.trim="search.name" class="search-name" size="small" :placeholder="`名称查询（${title}）`" clearable />
            <el-input
              v-model.trim="search.serialNumber"
              class="search-name"
              size="small"
              :placeholder="`编号查询（${title}}）`"
              clearable
              style="margin-left: 15px"
            />
          </div>
        </div>
      </template>
      <div class="card-body">
        <el-table
          ref="tableRef"
          :data="tableList"
          :max-height="maxHeight - 170"
          empty-text="暂无数据"
          highlight-current-row
          style="width: 100%"
          @row-click="(row, col, event) => rowClick(row, col, event, 1)"
          @selection-change="handleSelectionChange($event, 1)"
        >
          <el-table-column v-if="selected.delBtn" type="selection" width="45" align="center" />
          <el-table-column prop="name" label="名称" min-width="120" />
          <el-table-column prop="code" align="center" label="代码" />
          <el-table-column prop="attributeName" align="center" label="类型" />
        </el-table>
      </div>
    </el-card>
    <del-confirm
      v-model="visible.del"
      :tip="delTip"
      :fn="crudApi.del"
      :fn-data="selected.rows"
      show-exit
      @exit="delExit()"
      @success="handleDelSuccess"
    />
  </div>
</template>

<script setup>
import { defineProps, defineEmits, inject, ref, reactive, computed, watch } from 'vue'
import { isNotBlank, isBlank } from '@data-type/index'
import { dightLowercase } from '@data-type/number'

import DelConfirm from '@comp-common/del-confirm/index.vue'

const permission = inject('permission')
const crudApi = inject('crudApi')
const maxHeight = inject('maxHeight')
const selectMap = inject('selectMap')

const emit = defineEmits(['add', 'del'])
const props = defineProps({
  level: {
    type: Number,
    required: true
  },
  data: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const parentLevel = props.level - 1

// 表格ref
const tableRef = ref()

const search = reactive({
  name: '',
  serialNumber: ''
})

const selected = reactive({
  currentRow: null,
  rows: [],
  delBtn: false
})

const visible = reactive({
  del: false
})

const title = computed(() => {
  return `${dightLowercase(props.level)}级科目`
})

// 删除提示
const delTip = `此操作不可逆，确认删除【${title.value}】中选中的记录`

// 选中的父级科目
const selectedParent = computed(() => {
  return selectMap[`current_LV${parentLevel}`]
})

// 上级科目选中提示
const labelTip = computed(() => {
  const parent = selectedParent.value
  return isNotBlank(parent) ? `上级科目：${parent.basicClassName}-${parent.name}-${parent.code}` : null
})

// 上一个等级的所有的科目id
const selectedParentIds = computed(() => {
  const filterParentList = selectMap[`current_list_LV${parentLevel}`]
  if (selectedParent.value) { // 父级有选中的情况（高亮）
    return [selectedParent.value.id]
  } else if (filterParentList) { // 父级科目未选中的情况，父级的列表
    return filterParentList.map((v) => v.id)
  }
  return []
})

// 列表
const tableList = computed(() => {
  const list = props.data.filter(
    (v) =>
      selectedParentIds.value.includes(v.parent.id) &&
      v.name.includes(search.name) &&
      v.serialNumber.includes(search.serialNumber)
  )
  // 校验选中行是否依然被选中，未选中则取消
  whetherContainsCheckRow(list)
  return list
})

selectMap[`current_list_LV${props.level}`] = tableList

watch(tableList.value, (list) => {
  const selectedExit = isNotBlank(selected.currentRow) && list.some((l) => selected.currentRow.id !== l.id)
  // 不存在则取消选中
  if (!selectedExit) {
    selected.currentRow = null
  }
})

// 添加
function add() {
  emit('add', props.level)
}

// 删除
function del() {
  if (isNotBlank(selected.rows)) {
    visible.del = true
  } else {
    selected.delBtn = !selected.delBtn
  }
}

// 退出删除
function delExit() {
  tableRef.value.clearSelection()
  selected.delBtn = false
}

// 删除成功回调
function handleDelSuccess() {
  delExit()
  emit('del')
}

// 选中行，设置选中/取消选中
function rowClick(row) {
  // 选择已选中则取消选中
  if (isNotBlank(selected.currentRow) && selected.currentRow.id === row.id) {
    selected.currentRow = null
    tableRef.value.setCurrentRow()
  } else {
    selected.currentRow = row
  }
  selectMap[`current_LV${props.level}`] = selected.currentRow
}

function whetherContainsCheckRow(list) {
  if (isBlank(selected.currentRow)) return
  const checked = list.some(v => v.id === selected.currentRow.id)
  if (!checked) {
    uncheck()
  }
}

// 取消选中
function uncheck() {
  selected.currentRow = null
  tableRef.value.setCurrentRow()
  selectMap[`current_LV${props.level}`] = selected.currentRow
}

// 选择行
function handleSelectionChange(val) {
  selected.rows = val
}
</script>
