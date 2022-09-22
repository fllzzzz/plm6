<template>
  <div>
    <div v-show="!layingOffId">
      <div class="my-code">点击下料方式查看详情</div>
    </div>
    <div v-show="layingOffId">
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        highlight-current-row
        :empty-text="crud.emptyText"
        :max-height="maxHeight + 42"
        style="width: 100%"
        row-key="id"
        @current-change="handleCurrentChange"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="切割形式"
          align="center"
          min-width="140px"
        />
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" :permission="permission" />
          </template>
        </el-table-column>
      </common-table>
      <!-- 表单 -->
      <mForm />
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/cutting-config'
import { defineExpose, ref, defineProps, defineEmits, watch, computed, inject, provide } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import { configProductionLineGroupPM as permission } from '@/page-permission/config'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'

const emit = defineEmits(['click-cut-config'])
// 是否默认初始行
const defaultFirstRow = ref(false)
const currentRowId = ref()

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '切割方式配置',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)
provide('crud', crud)
const maxHeight = inject('maxHeight')

const props = defineProps({
  layingOffRow: {
    type: Object,
    default: () => {}
  }
})

const layingOffId = computed(() => {
  return props.layingOffRow && props.layingOffRow.id
})

watch(
  () => layingOffId,
  (val) => {
    if (val.value) {
      defaultFirstRow.value = true
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

function handleCurrentChange(val) {
  currentRowId.value = val.id
  console.log(val, 'cutConfig-click')
  emit('click-cut-config', val)
}

// 设置选中行
function setCurrentRow(val) {
  tableRef.value?.setCurrentRow(val)
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.id = layingOffId.value
  return !!crud.query.id
}

CRUD.HOOK.beforeToQuery = () => {
  crud.query.id = layingOffId.value
  return !!crud.query.id
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    if (defaultFirstRow.value && i === 0) {
      setCurrentRow(v)
    }
    if (!defaultFirstRow.value && currentRowId.value === v.id) {
      setCurrentRow(v)
    }
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.layingWayId = layingOffId.value
}

function refresh() {
  defaultFirstRow.value = false
  crud.toQuery()
}

defineExpose({
  permission,
  refresh,
  toAdd: crud.toAdd
})
</script>
