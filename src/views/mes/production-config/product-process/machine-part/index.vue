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
       <el-table-column prop="specSequence" :show-overflow-tooltip="true" label="规格前缀索引" min-width="500"></el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/steel-classic'
import { ref } from 'vue'
import { configProductProcessMachinePartPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'

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
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.specSequence = v.links.map(v => `${v.keyword}【${v.specIndex ? v.specIndex : '全部'}】`).join('、')
    return v
  })
}
</script>
