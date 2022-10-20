<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader ref="headRef" :fInfo="fInfo">
        <template #auditBox>
          <slot name="auditBox"></slot>
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="productFormat[productType]"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="rowId"
      style="width: 100%"
      @selection-change="handleSelectionChange"
    >
      <el-table-column type="selection" align="center" width="60" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="编号"
        align="center"
        :width="fixedWidth ? '80px' : ''"
        :min-width="!fixedWidth ? '80px' : ''"
        :fixed="fixed"
      >
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('specification')"
        :show-overflow-tooltip="true"
        prop="specification"
        label="规格"
        align="center"
        :width="fixedWidth ? '80px' : ''"
        :min-width="!fixedWidth ? '80px' : ''"
        :fixed="fixed"
      >
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        :show-overflow-tooltip="true"
        prop="quantity"
        label="数量"
        align="center"
        :width="fixedWidth ? '80px' : ''"
        :min-width="!fixedWidth ? '80px' : ''"
        :fixed="fixed"
      >
        <template #default="{ row }">
          <span>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('weight')"
        :show-overflow-tooltip="true"
        prop="weight"
        label="重量（kg）"
        align="center"
        :width="fixedWidth ? '80px' : ''"
        :min-width="!fixedWidth ? '80px' : ''"
        :fixed="fixed"
      >
        <template #default="{ row }">
          <span>{{ row.weight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-permission="permission.edit" align="center" prop="prop" label="操作" width="110">
        <template #default="{ row }">
          <common-button type="warning" size="mini" @click="handleSingleEdit(row)">工价调整</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/wages-adjust/detail'
import { ref, defineProps, defineExpose, defineEmits, inject, computed, watch } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'
import { productFormat } from '@/utils/columns-format/mes'
// import productTypeSpecInfoColumns from '@comp-mes/table-columns/productType-spec-info-columns'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const emit = defineEmits(['setInfo', 'clearInfo', 'selection-change'])

const permission = inject('permission')
const props = defineProps({
  fQuery: {
    type: Object,
    default: () => {}
  },
  fInfo: {
    type: Object,
    default: () => {}
  }
})

const productType = computed(() => {
  return props.fQuery?.productType || componentTypeEnum.MACHINE_PART.V
})

watch(
  () => productType.value,
  () => {
    crud.data = []
    emit('clearInfo')
  }
)

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const headRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工价调整明细',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true,
    queryOnPresenterCreated: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.beforeRefresh = () => {
  const { name, steelId, steelSpec, category } = props.fInfo || {}
  crud.query = Object.assign(crud.query, props.fQuery, { name, steelId, steelSpec, category })
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    return v
  })
}

function handleSingleEdit(row) {
  emit('setInfo', row)
}

function handleSelectionChange(val) {
  emit('selection-change', val)
}

defineExpose({
  toQuery: crud.toQuery
})
</script>
