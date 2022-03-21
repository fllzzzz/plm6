<template>
    <div class="app-container">
    <!-- 工具栏 -->
    <mHeader />
    项目数据

<!--表格渲染-->
    <common-table
    ref="tableRef"
    v-loading="crud.loading"
    :data="crud.data"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="费用类型名称" min-width="150">
      <template v-slot="scope">
        <div>{{ scope.row.name }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" :show-overflow-tooltip="true" label="排序" min-width="80">
      <template v-slot="scope">
        <span>{{ scope.row.sort }}</span>
      </template>
    </el-table-column>

  </common-table>

  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import mHeader from './module/header'
import crudApi from '@/api/cutting/project-data'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'

const tableRef = ref()

// crud交由presenter持有
const permission = {
  get: ['expenseConfig:get']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, columns, CRUD } = useCRUD(
  {
    title: '费用归类',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    console.log('v', v)
  })
}
</script>
