<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="name" align="center" :show-overflow-tooltip="true" label="零件类型" min-width="200"></el-table-column>
      <el-table-column prop="specSequence" :show-overflow-tooltip="true" label="规格前缀索引" min-width="300"></el-table-column>
      <el-table-column prop="processSequence" :show-overflow-tooltip="true" label="工序" min-width="400"> </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :showDel="false" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { getMachinePart } from '@/api/mes/production-config/product-process'
import { ref } from 'vue'
import { configProductProcessMachinePartPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '零件工序定义',
    sort: [],
    hasPagination: false,
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, get: getMachinePart }
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.specSequence = v.links.map((v) => `${v.keyword}【${v.specIndex ? v.specIndex : '全部'}】`).join('、')
    v.processSequence = v.productProcessLinkList?.map((v) => `【${v.name}】`).join('→')
    v.processSequenceIds = v.productProcessLinkList?.map((v) => v.processId)
    return v
  })
}
</script>
