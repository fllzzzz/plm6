<template>
  <div>
    <el-card :style="{ height: `${maxHeight}px` }">
      <template #header>
        <div class="card-header flex-css">
          <div class="flex-rbc">
            <span>{{ title }}</span>
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
            <common-select
              v-model="search.basicClass"
              :options="classificationEnum"
              clearable
              type="enum"
              size="small"
              placeholder="材料类型"
              class="search-type"
            />
          </div>
        </div>
      </template>
      <div class="card-body">
        <common-table
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
          <el-table-column prop="basicClassName" align="center" label="类型" />
        </common-table>
      </div>
    </el-card>
    <del-confirm
      v-model="visible.del"
      :tip="delTip"
      :fn="delRow"
      :fn-data="selected.rows"
      show-exit
      @exit="delExit()"
      @success="handleDelSuccess"
    />
  </div>
</template>

<script setup>
import { defineProps, defineEmits, inject, ref, reactive, computed, watch } from 'vue'
import { classificationEnum } from '@enum-ms/classification'
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
    default: 1
  },
  data: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

// 表格ref
const tableRef = ref()

const search = reactive({
  name: '',
  basicClass: undefined
})

const selected = reactive({
  checkRow: null,
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

const tableList = computed(() => {
  const list = props.data.filter((v) => v.name.includes(search.name) && (isBlank(search.basicClass) || v.basicClass === search.basicClass))
  // 校验选中行是否依然被选中，未选中则取消
  whetherContainsCheckRow(list)
  return list
})

selectMap[`current_list_LV${props.level}`] = tableList

watch(tableList.value, (list) => {
  const selectedExit = isNotBlank(selected.checkRow) && list.some((l) => selected.checkRow.id !== l.id)
  // 不存在则取消选中
  if (!selectedExit) {
    selected.checkRow = null
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

// 删除方法
async function delRow() {
  try {
    const rowIds = selected.rows.map(row => row.id)
    await crudApi.del(rowIds)
  } catch (error) {
    console.log('删除科目', error)
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
  if (isNotBlank(selected.checkRow) && selected.checkRow.id === row.id) {
    selected.checkRow = null
    tableRef.value.setCurrentRow()
  } else {
    selected.checkRow = row
  }
  selectMap[`current_LV${props.level}`] = selected.checkRow
}

function whetherContainsCheckRow(list) {
  if (isBlank(selected.checkRow)) return
  const checked = list.some(v => v.id === selected.checkRow.id)
  if (!checked) {
    uncheck()
  }
}

// 取消选中
function uncheck() {
  selected.checkRow = null
  tableRef.value.setCurrentRow()
  selectMap[`current_LV${props.level}`] = selected.checkRow
}

// 选择行
function handleSelectionChange(val) {
  selected.rows = val
}
</script>
