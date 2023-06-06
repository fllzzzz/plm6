<template>
  <div>
    <div v-show="!lineId">
      <div class="my-code">点击生产线查看详情</div>
    </div>
    <div v-show="lineId">
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
          label="生产组名称"
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
      <!--分页组件-->
      <pagination />
      <mForm :productType="line.productType" />
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/bridge/production-config/production-line-group'
import { defineExpose, ref, defineProps, defineEmits, watch, computed, inject } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { bridgeConfigProductionLineGroupPM as permission } from '@/page-permission/config'

import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mForm from './module/form'

const emit = defineEmits(['click-group'])

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '生产组',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

const maxHeight = inject('maxHeight')

const props = defineProps({
  line: {
    type: Object,
    default: () => {}
  }
})

const lineId = computed(() => {
  return props.line && props.line.id
})

watch(
  () => lineId,
  (val) => {
    if (val.value) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

function handleCurrentChange(val) {
  if (val) {
    emit('click-group', val)
  }
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.productionLineId = lineId.value
  return !!crud.query.productionLineId
}

CRUD.HOOK.beforeToQuery = () => {
  crud.query.productionLineId = lineId.value
  return !!crud.query.productionLineId
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.productionLineId = lineId.value
  crud.form.factoryId = props.line.factoryId
  crud.form.workshopId = props.line.workshopId
}

defineExpose({
  permission,
  toAdd: crud.toAdd
})
</script>
