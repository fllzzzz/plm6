<template>
  <el-card class="line-box box-card">
    <template v-slot:header>
      <span style="line-height: 28px">{{ crud.title }}列表</span>
    </template>
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      highlight-current-row
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      @current-change="handleCurrentChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
       <el-table-column
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="140px"
      />
      <el-table-column
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="描述"
        min-width="140px"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </el-card>
</template>

<script setup>
import crudApi from '@/api/system/dict'
import { ref, defineEmits } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const emit = defineEmits(['click-line'])

// crud交由presenter持有
const permission = {
  get: ['systemDict:get'],
  editStatus: ['systemDict:editStatus']
}

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const multipleTable = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '字典列表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef,
  multipleTable
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.dict-box',
  paginate: true,
  extraHeight: 157
})

function handleCurrentChange(val) {
  if (val) {
    emit('click-line', val)
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  if (data.data.content.length > 0) {
    data.data.content.forEach(v => {
      v.type = crud.query.type
    })
  }
  return data
}
</script>

<style lang="scss" scoped>
::deep(.line-box) {
  .el-card__body {
    padding-top: 11px;
    .el-tabs {
      margin-bottom: 7px;
    }
  }
  .card-header {
    height: 28px;
  }
}
</style>
