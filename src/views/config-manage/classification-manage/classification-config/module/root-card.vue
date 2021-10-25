<template>
  <el-card v-bind="$attrs" :style="{ height: `${maxHeight}px` }">
    <template #header>
      <div class="card-header flex-css">
        <div class="flex-rbc">
          <span>一级科目</span>
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
          <el-input v-model.trim="search.name" class="search-name" size="small" placeholder="名称查询（一级科目）" clearable />
          <common-select
            v-model:value="search.basicClass"
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
      <el-table
        :ref="tableRef"
        :data="tableList"
        :max-height="maxHeight - 130"
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
    @exit="delExit()"
    @success="handleDelSuccess"
  />
</template>

<script setup>
import DelConfirm from '@comp-common/del-confirm/index.vue'

import { defineProps, defineEmits, inject, ref, reactive, computed, watch } from 'vue'
import { isNotBlank, isBlank } from '@/utils/data-type'
import { classificationEnum } from '@enum-ms/classification'

const permission = inject('permission')
const crudApi = inject('crudApi')
const maxHeight = inject('maxHeight')

const emit = defineEmits(['add', 'del'])
const props = defineProps({
  title: {
    type: String,
    default: '一级科目'
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
// 删除提示
const delTip = `此操作不可逆，确认删除【${props.title}】中选中的记录`

const search = reactive({
  name: undefined,
  basicClass: undefined
})

const selected = reactive({
  currentRow: null,
  rows: [],
  delBtn: false
})

const visible = reactive({
  del: false
})

const tableList = computed(() => {
  return props.data.filter((v) => v.name.includes(search.name) && (isBlank(this.basicClass) || v.basicClass === search.basicClass))
})

watch(tableList.value, (list) => {
  const selectedExit = isNotBlank(selected.currentRow) && list.some((l) => selected.currentRow.id !== l.id)
  // 不存在则取消选中
  if (!selectedExit) {
    selected.currentRow = null
  }
})

// 添加
function add() {
  emit('add')
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
}

// 选择行
function handleSelectionChange(val) {
  selected.rows = val
}
</script>
