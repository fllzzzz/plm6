<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @row-click="handleRowChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        show-overflow-tooltip
        :label="`${componentTypeEnum.VL[crud.query.taskTypeEnum]}类型`"
        prop="productClass.name"
        align="center"
      />
      <el-table-column label="数量（件）" prop="quantity" width="110" align="center"></el-table-column>
      <el-table-column label="重量（吨）" prop="netWeight" width="110" align="center"></el-table-column>
      <el-table-column v-if="checkPermission(permission.edit)" align="center" prop="prop" label="操作" width="100">
        <template #default="{ row }">
          <common-button type="warning" size="mini" @click="handleEdit(row)">修改</common-button>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { typeGet } from '@/api/mes/production-line-wage-statistics/wage-adjust'
import { ref, defineExpose, defineEmits, inject } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const emit = defineEmits(['setInfo'])

const permission = inject('permission')
const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '工价调整汇总',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: typeGet },
    sort: [],
    hasPagination: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: false })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.netWeight = convertUnits(v.netWeight, 'kg', 't', DP.COM_WT__T)
  })
}

function handleEdit(row) {
  emit('setInfo', row)
}

function handleRowChange(val) {
  emit('setInfo', val)
}

defineExpose({
  query: crud.query
})
</script>
